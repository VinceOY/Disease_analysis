rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
library(data.table)
library(stringr)
library(survival)
# check data density plot
#===============================================================================
# set parameter
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/step4/",
  output_path = "C:/Users/USER/Downloads/proj_data/step5/",
  Test_item = c("HbA1c", "ALBUMIN", "Uric", "HDL","LDL","Creatinine","Triglyceride"),
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
)
input_path <- parameters$input_path
output_path <- parameters$output_path
Test_item <- parameters$Test_item
outcome_diseases <- parameters$outcome_diseases
#===============================================================================
# 定義加權標準差函數
# need to know n(m): 每一個ID每一個月份的test data數量 n是筆數, m是月份
# 撈出id, interval, month 

# 定義 VIM 計算函數
calculate_vim <- function(x, lambda = 1) {
  sd_value <- sd(x, na.rm = TRUE)
  mean_value <- mean(x, na.rm = TRUE)
  vim <- sd_value / (mean_value^lambda)
  return(vim)
}

adj_sd <- function(x) {
  n <- length(x[!is.na(x)])
  adj_sd <- sd(x, na.rm = TRUE) * sqrt((n - 1) / n)
  return(adj_sd)
}

# t <- Test_item[3]
# o <- outcome_diseases[6]
for (t in Test_item) {
  for (o in outcome_diseases) {
    d_tmp <- fread(paste0(input_path,t,"_",o,"_dtf.csv"))
    
    
    select_col <- c("ID", "Index_date", "Test_date", "numeric_value", "unit",
                    "interval")
    dt_v <- d_tmp[,..select_col]
    dt_v <- dt_v[!is.na(interval)]
    
    dt_v <- dt_v[, .(
      mean_value = mean(numeric_value, na.rm = TRUE)), by = .(ID, interval)]
    
    dt_v <- dcast(dt_v, ID ~ interval, value.var = c("mean_value"))
    interval_cols <- paste0("mean",0:(length(unique(d_tmp$interval))-2))
    setnames(dt_v, old = names(dt_v)[-1], new = interval_cols)

    dt_v[, sd_value := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = interval_cols]
    dt_v[, cv_value := apply(.SD, 1, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)),
         .SDcols = interval_cols]
    dt_v[, rms_value := apply(.SD, 1, function(x) sqrt(mean(x^2, na.rm = TRUE))), 
         .SDcols = interval_cols]
    
    # vim
    dt_v[, vim_value := apply(.SD, 1, calculate_vim, lambda = 0.3),
         .SDcols = interval_cols]
    
    # adj_sd
    dt_v[, adj_sd := apply(.SD, 1, adj_sd), .SDcols = interval_cols]
    
    # d_tmps beta
    beta_list <- list()
    for (i in unique(d_tmp$ID)) {
      
      dp <- d_tmp[ID==i]
      dp <- dp[!is.na(interval)]
      model <- lm(numeric_value ~ followup, data = dp)
      dp[,.(followup,numeric_value)]
      
      beta_list[[i]] <- coef(model)[["followup"]]
    }
    
    # unlist cor_ID to dt
    beta_df <- data.table(ID = names(beta_list), beta = unlist(beta_list))
    
    # merge to original dt
    d_tmp <- merge(d_tmp, beta_df, by = "ID", all.x = TRUE)
    
    select_col2 <- c("ID", "SEX_TYPE", "Index_year", "AGE","AGE_GROUP", 
                     "Hypertension_event","PeripheralEnthe_event", 
                     "UnknownCauses_event", "LipoidMetabDis_event",
                     "AcuteURI_event", "AbdPelvicSymptoms_event", 
                     "Dermatophytosis_event","GenSymptoms_event", 
                     "RespChestSymptoms_event", "HeadNeckSymptoms_event", 
                     "ContactDermEczema_event", "ViralInfection_event", 
                     "ObesityHyperal_event","JointDisorders_event", 
                     "AcuteBronchitis_event", "SoftTissueDis_event",
                     "BloodExamFindings_event", "RefractionDis_event", 
                     "ConjunctivaDis_event", paste0(o, "_event"), 
                     paste0(o, "_followup"),"beta")
    
    d_tmp[,Index_year:= year(Index_date)] 
    d_tmp <- d_tmp[,..select_col2]
    d_tmp <- d_tmp[!duplicated(d_tmp), ]
    d_tmp <- merge(d_tmp, dt_v, by = "ID", all = T)
    
    # output: basic, outcome, mean, variation score by ppl * 12
    csv_file_name <- paste0(output_path, t,"_",o,"_dtf_all.csv")
    fwrite(d_tmp, file = csv_file_name, row.names = FALSE)
  }
}

#===============================================================================
# build model table: 
inputs <- list(option1 = c(),
               option2 = c("mean0","sd_value"),
               option3 = c("mean0","cv_value"),
               option4 = c("mean0","rms_value"),
               option5 = c("mean0","vim_value"),
               option6 = c("mean0","adj_sd"))
model_result <- data.table()
m <- 1

for(o in outcome_diseases){
  for(t in Test_item){
    # prepare y 
    file_names <- paste0(output_path, t, "_", o, "_dtf_all.csv")
    dt <- fread(file_names)
    
    # transfer to cate
    #dt <- dt[, mean_fact :=  ifelse(mean0  < 3.4 | mean0  > 6, 
    #                                ifelse(mean0  < 3.4, 1, 2), 0)]
     
    category_col <- c("SEX_TYPE", "Index_year", "AGE", "Hypertension_event",      
                      "PeripheralEnthe_event", "UnknownCauses_event", 
                      "LipoidMetabDis_event", "AcuteURI_event", 
                      "AbdPelvicSymptoms_event", "Dermatophytosis_event", 
                      "GenSymptoms_event", "RespChestSymptoms_event",
                      "HeadNeckSymptoms_event", "ContactDermEczema_event",
                      "ViralInfection_event", "ObesityHyperal_event",   
                      "JointDisorders_event", "AcuteBronchitis_event",
                      "SoftTissueDis_event", "BloodExamFindings_event",
                      "RefractionDis_event", "ConjunctivaDis_event")
    
    dt[, (category_col) := lapply(.SD, as.factor), .SDcols = category_col]
    dt$AGE <- as.integer(dt$AGE)
    
    # test exclude beta, CV
    dt <- dt[abs(beta) < quantile(dt$beta,0.9)]

    for(i in names(inputs)){
      select_col <- c("SEX_TYPE", "Index_year", "AGE", "Hypertension_event",
                      "PeripheralEnthe_event", "LipoidMetabDis_event",   
                      "AcuteURI_event", "AbdPelvicSymptoms_event", 
                      "Dermatophytosis_event", "GenSymptoms_event", 
                      "RespChestSymptoms_event", "HeadNeckSymptoms_event", 
                      "ContactDermEczema_event", "ViralInfection_event",
                      "ObesityHyperal_event", "AcuteBronchitis_event",  
                      "SoftTissueDis_event", "BloodExamFindings_event", 
                      "ConjunctivaDis_event" , paste0(o, "_event"), 
                      paste0(o, "_followup"))
      
      select_col <- c(select_col, inputs[[i]])
      dt_input <- dt[,..select_col]
      
      # 刪除只有一種類別的欄位
      factor_columns <- sapply(dt_input, is.factor)
      check_level_factors <- sapply(dt_input[,..factor_columns], 
                                    function(x) length(unique(x)))
      for (col in names(check_level_factors)) {
        if (length(levels(dt_input[[col]])) < 2) {
          dt_input[[col]] <- NULL  
        }
      }
      
      time_col <- paste0(o, "_followup")
      event_col <- paste0(o, "_event")
      
      # build model
      #var <- paste('Surv(',t,',',e,')~',col, '+')
      #as.formula(var)
      
      cox_model <- coxph(Surv(dt_input[[time_col]], dt_input[[event_col]]) ~ ., 
                         data = dt_input[, !c(time_col, event_col), with = FALSE])
      
      # Generate summary
      summary_cox <- summary(cox_model)
      
      # Extract relevant information for the table
      var <- names(cox_model$coefficients)
      HR <- round(exp(cox_model$coefficients),3)  # Hazard ratios
      CI <- list()
      for (i in 1:length(summary_cox$conf.int[,1])) {
        CI[[i]] <- c(round(summary_cox$conf.int[i, 3], 3), 
                     round(summary_cox$conf.int[i, 4], 3))  
      }
      p_value <- summary_cox$coefficients[, 5]  # p-value
      significance <- ifelse(p_value < 0.001, "***", 
                             ifelse(p_value < 0.01, "**", 
                                    ifelse(p_value < 0.05, "*", 
                                           ifelse(p_value < 0.1, ".", ""))))
      # Create a new table for the output
      output_table <- data.table(
        model = paste0("model", m),
        test_item = t, 
        outcome = o,
        n = nrow(dt_input),
        Variable = var,
        HR = HR,
        CI = CI,
        p_value = significance
      )
      model_result <- rbind(model_result, output_table)
      m = m + 1
    }
  }
}

csv_file_name <- paste0(output_path, "model_summary_clean.csv")
fwrite(model_result, file = csv_file_name, row.names = FALSE)


