rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
library(data.table)
library(stringr)
library(survival)

#===============================================================================
# define parameters: 
# Test_item: test_item name
# target_folder_path: 檔案存放路徑
# outcome_diseases: DM related diseases
parameters <- list(
  target_folder_path = "C:/Users/USER/Downloads/model_analysis/",
  exclude_columns = c("exclude_AGE", "exclude_ID", "exclude_Indexdate",
                      "exclude_outcome1","exclude_outcome2","exclude_valid_ID"),
  Test_item = c("HbA1c", "ALBUMIN"),
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
)
target_folder_path <- parameters$target_folder_path
exclude_columns <- parameters$exclude_columns
outcome <- parameters$outcome_diseases
Test_item <- parameters$Test_item
#=================================================================
# 獲取資料夾中所有文件名稱
create_intervals <- function(values, num_interval, tracking_interval, event_interval) {
  intervals <- rep(NA, length(values))
  for (m in 0:(num_interval-1)) {
    lower <- m * tracking_interval - event_interval + 1
    upper <- m * tracking_interval + event_interval
    intervals[values >= lower & values <= upper] <- paste(m)
  }
  return(intervals)
}
file_list <- list.files(path = target_folder_path)

for (t in Test_item) {
  for (o in outcome) {
    file_names <- paste0(target_folder_path,t,"_",o,".csv") # clean_df
    d_tmp <- fread(file_names)
    d_tmp <- d_tmp[apply(d_tmp[, ..exclude_columns], 1, sum) < 1]
    
    # cal mean, median , sd by ID, season
    select_col <- c("ID", "Index_date", "Test_date", "numeric_value", "unit")
    
    dt_v <- d_tmp[,..select_col]
    dt_v <- dt_v[, followup := as.numeric(Test_date-Index_date)]
    dt_v[, interval := create_intervals(dt_v$followup, 5, 90, 45)]
    dt_v <- dt_v[!is.na(interval)] 

    dt_v <- dt_v[, .(
      mean_value = mean(numeric_value, na.rm = TRUE)), by = .(ID, interval)]
    dt_v <- dcast(dt_v, ID ~ interval, value.var = c("mean_value"))
    interval_cols <- paste0("mean",0:4)
    setnames(dt_v, old = names(dt_v)[-1], new = interval_cols)
    
    # cal sd, cv, rms by ppl 
    dt_v[, sd_value := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = interval_cols]
    dt_v[, cv_value := apply(.SD, 1, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)), .SDcols = interval_cols]
    dt_v[, rms_value := apply(.SD, 1, function(x) sqrt(mean(x^2, na.rm = TRUE))), .SDcols = interval_cols]
    
    select_col <- c("ID", "SEX_TYPE", "Index_year", "AGE_GROUP", 
                    "Hypertension_event","PeripheralEnthe_event", 
                    "UnknownCauses_event", "LipoidMetabDis_event",
                    "AcuteURI_event", "AbdPelvicSymptoms_event", 
                    "Dermatophytosis_event","GenSymptoms_event", 
                    "RespChestSymptoms_event", "HeadNeckSymptoms_event", 
                    "ContactDermEczema_event", "ViralInfection_event", 
                    "ObesityHyperal_event","JointDisorders_event", 
                    "AcuteBronchitis_event", "SoftTissueDis_event",
                    "BloodExamFindings_event", "RefractionDis_event", 
                    "ConjunctivaDis_event")
    
    d_tmp[,Index_year:= format(d_tmp$Index_date, "%Y")] 
    d_tmp <- d_tmp[,..select_col]
    d_tmp <- d_tmp[!duplicated(d_tmp), ]
    d_tmp <- merge(d_tmp, dt_v, by = "ID", all = T)
    csv_file_name <- paste0(target_folder_path, t,"_",o,"_dtf.csv")
    fwrite(d_tmp, file = csv_file_name, row.names = FALSE)
  }
}

#===============================================================================
# build model table: 
inputs <- list(option1 = c(),
               option2 = c("sd_value"),
               option3 = c("cv_value"),
               option4 = c("rms_value")
               )

model_result <- data.table()
m <- 1
for(o in outcome){
  # prepare y 
  file_names_y <- paste0(target_folder_path, o, "_clean.csv")
  dt_y <- fread(file_names_y)
  select_y <- c("ID", paste0(o, "_event"), paste0(o, "_followup"))
  dt_y <- dt_y[,..select_y]
  
  for(t in Test_item){
    # prepare x and merge with y 
    file_names_x <- paste0(target_folder_path, t, "_", o, "_dtf.csv")
    dt_x <- fread(file_names_x)
    category_col <- c("SEX_TYPE", "Index_year", "AGE_GROUP", "Hypertension_event",      
                      "PeripheralEnthe_event", "UnknownCauses_event", 
                      "LipoidMetabDis_event", "AcuteURI_event", 
                      "AbdPelvicSymptoms_event", "Dermatophytosis_event", 
                      "GenSymptoms_event", "RespChestSymptoms_event",
                      "HeadNeckSymptoms_event", "ContactDermEczema_event",
                      "ViralInfection_event", "ObesityHyperal_event",   
                      "JointDisorders_event", "AcuteBronchitis_event",
                      "SoftTissueDis_event", "BloodExamFindings_event",
                      "RefractionDis_event", "ConjunctivaDis_event")
    
    dt_x[, (category_col) := lapply(.SD, as.factor), .SDcols = category_col]
    dt_T <- merge(dt_y, dt_x, by = "ID", all.y = T)
    
    # 檢查data中所有類別變數是否有多類別
    factor_columns <- sapply(dt_T, is.factor)
    single_level_factors <- sapply(dt_T[,..factor_columns], 
                                   function(x) length(unique(x)))
    for (col in names(single_level_factors)) {
      if (length(levels(dt_T[[col]])) < 2) {
        dt_T[[col]] <- NULL  
      }
    }
    
    for(i in names(inputs)){
      # select col
      select_col <- c(names(dt_T)[2:19], inputs[[i]])
      dt_T_X <- dt_T[,..select_col]
      col_y <- c(paste0(o, "_followup"), paste0(o, "_event"))
      
      time_col <- col_y[1]
      event_col <- col_y[2]
      
      # build model
      cox_model <- coxph(Surv(dt_T_X[[time_col]], dt_T_X[[event_col]]) ~ ., 
                         data = dt_T_X[, !c(time_col, event_col), with = FALSE])

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
        model = paste0("model",m),
        test_item = t, 
        outcome = o,
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

csv_file_name <- paste0(target_folder_path, "model_summary.csv")
fwrite(model_result, file = csv_file_name, row.names = FALSE)


