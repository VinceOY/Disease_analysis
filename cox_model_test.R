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
    file_names <- paste0(target_folder_path,t,"_",o,".csv")
    d_tmp <- fread(paste0(target_folder_path, matched_files[1]))
    d_tmp <- d_tmp[apply(d_tmp[, ..exclude_columns], 1, sum) < 1]
    
    # cal mean, median , sd by ID, season
    select_col <- c("ID", "Index_date", "Test_date", "numeric_value","unit")
    
    dt_v <- d_tmp[,..select_col]
    dt_v <- dt_v[, followup := as.numeric(Test_date-Index_date)]
    dt_v[, interval := create_intervals(dt_v$followup, 5, 90, 45)]
    dt_v <- dt_v[!is.na(interval)] 
    
    dt_v <- dt_v[, .(
      mean_value = mean(numeric_value, na.rm = TRUE),
      sd_value = sd(numeric_value, na.rm = TRUE),
      rms_value = sqrt(mean(numeric_value^2, na.rm = TRUE)),
      cv_value = sd(numeric_value, na.rm = TRUE) / mean(numeric_value, na.rm = TRUE)
    ), by = .(ID, interval)]
    dt_v <- dcast(dt_v, ID ~ interval, 
                  value.var = c("mean_value", "sd_value", "rms_value", "cv_value"))
    select_col <- c("ID", "SEX_TYPE", "Index_year", "AGE_GROUP", "Hypertension_event",
                    "PeripheralEnthe_event", "UnknownCauses_event", "LipoidMetabDis_event",
                    "AcuteURI_event", "AbdPelvicSymptoms_event", "Dermatophytosis_event",
                    "GenSymptoms_event", "RespChestSymptoms_event", "HeadNeckSymptoms_event", 
                    "ContactDermEczema_event", "ViralInfection_event", "ObesityHyperal_event",
                    "JointDisorders_event", "AcuteBronchitis_event", "SoftTissueDis_event",
                    "BloodExamFindings_event", "RefractionDis_event", "ConjunctivaDis_event")
    
    d_tmp[,Index_year:= format(d_tmp$Index_date, "%Y")] 
    d_tmp <- d_tmp[,..select_col]
    d_tmp <- d_tmp[!duplicated(d_tmp), ]
    d_tmp <- merge(d_tmp, dt_v, by = "ID", all = T)
    csv_file_name <- paste0(target_folder_path, t,"_",o,"_dtf.csv")
    fwrite(d_tmp, file = csv_file_name, row.names = FALSE)
  }
}

#===============================================================================
# build model table: 要加only mean
model_result <- data.table()
inputs <- list(option1 = c("sd_value_0", "sd_value_1", "sd_value_2", 
                           "sd_value_3", "sd_value_4"),
               option2 = c("cv_value_0", "cv_value_1", "cv_value_2", 
                           "cv_value_3", "cv_value_4"),
               option3 = c("rms_value_0", "rms_value_1", "rms_value_2", 
                           "rms_value_3", "rms_value_4")
               )
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
    dt_T <- dt_T[,-1]
    # 檢查數據框中所有因子變量是否有多個層次
    factor_columns <- sapply(dt_T, is.factor)
    single_level_factors <- sapply(dt_T[,..factor_columns], 
                                   function(x) length(unique(x)))
    for (col in names(single_level_factors)) {
      if (length(levels(dt_T[[col]])) < 2) {
        dt_T[[col]] <- NULL  # 刪除只有一個層次的變量
      }
    }
    
    for(i in names(inputs)){
      # select col
      select_col <- c(names(dt_T)[1:22], inputs[[i]])
      dt_T_X <- dt_T[,..select_col]
      dt_T_X <- dt_T_X[rowSums(is.na(dt_T_X)) == 0, ]
      col_y <- c(paste0(o, "_followup"), paste0(o, "_event"))
      cox_model <- coxph(Surv(get(col_y[1]), get(col_y[2])) ~ ., data = dt_T_X)
    
      # Generate summary
      summary_cox <- summary(cox_model)
      
      # Extract relevant information for the table
      var <- names(cox_model$coefficients)
      HR <- exp(cox_model$coefficients)  # Hazard ratios
      CI <- list()
      for (i in 1:length(summary_cox$conf.int[,1])) {
        CI[[i]] <- c(summary_cox$conf.int[i, 3], summary_cox$conf.int[i, 4])  
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


