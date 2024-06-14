rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/standard_function.R")
source("tool_function/tableone.R")
library(data.table)
library(stringr)
#===============================================================================
# define parameters: 
# Test_item: test_item name, unit
# exclude_columns: 清除age, ID, index_date
# target_folder_path: 檔案存放路徑
# outcome_diseases: DM related diseases
parameters <- list(
  target_folder_path = "C:/Users/USER/Downloads/disease_df/",
  Test_item = list(HbA1c = list(ID = c("014701","F09006B"), unit = c("%")),
                   ALBUMIN = list(ID = c("010301","11D101","F09038C"), 
                                  unit = c("(?i) g/dl"))),
  exclude_columns = c("exclude_AGE", "exclude_ID", "exclude_Indexdate"),
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
)
exclude_columns <- parameters$exclude_columns
target_folder_path <- parameters$target_folder_path
outcome <- parameters$outcome_diseases
Test_item <- parameters$Test_item
outcome_file <- paste0(outcome[1],"_clean.csv") # for table1 & table2 

#===============================================================================
# create table1
outcome_file <- paste0(outcome[1],"_clean.csv")
dt_outcome <- fread(paste0(target_folder_path, outcome_file))
# exclude AGE
exclude_columns = c("exclude_AGE", "exclude_ID", "exclude_Indexdate")
dt_outcome <- dt_outcome[apply(dt_outcome[, ..exclude_columns], 1, sum) < 1]
dt_outcome[,year:= format(dt_outcome$Index_date, "%Y")] 
continuous_col <- c("AGE")
category_col <- c("year","SEX_TYPE", "AGE_GROUP", "Hypertension_event",      
                  "PeripheralEnthe_event", "UnknownCauses_event", 
                  "LipoidMetabDis_event", "AcuteURI_event", 
                  "AbdPelvicSymptoms_event", "Dermatophytosis_event", 
                  "GenSymptoms_event", "RespChestSymptoms_event",
                  "HeadNeckSymptoms_event", "ContactDermEczema_event",
                  "ViralInfection_event", "ObesityHyperal_event",   
                  "JointDisorders_event", "AcuteBronchitis_event",
                  "SoftTissueDis_event", "BloodExamFindings_event",
                  "RefractionDis_event", "ConjunctivaDis_event")

dt_outcome[, (category_col) := lapply(.SD, as.factor), .SDcols = category_col]
tb1 <- create.table1(dt_outcome, need.col = c(continuous_col,category_col))
csv_file_name <- paste0(target_folder_path,"table1_basic.csv") # excel 
fwrite(tb1, file = csv_file_name, row.names = FALSE)

#===============================================================================
# create table2
# input: lab_result_swt.csv, outcome_clean(any outcome files)
create_intervals <- function(values, num_interval, tracking_interval, event_interval) {
  intervals <- rep(NA, length(values))
  for (m in 0:(num_interval-1)) {
    lower <- m * tracking_interval - event_interval + 1
    upper <- m * tracking_interval + event_interval
    intervals[values >= lower & values <= upper] <- paste(m)
  }
  return(intervals)
}

file_name <- paste0(target_folder_path,"lab_result_swt.csv")
dt_lab <- fread(file_name)
for (t in names(Test_item)) {
  Test_ID <- Test_item[[t]]$ID
  unit_p <- Test_item[[t]]$unit
  
  # select test_item
  dt_test <- dt_lab[grepl(paste0("^", paste(Test_ID, collapse="|^")), 
                            Test_item)]
  # clean test values
  dt_test <- dt_test[, `:=`(clean_value = str_replace_all(VALUE, unit_p, ""), 
                            unit = unit_p)]
  dt_test <- dt_test[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
  dt_test <- dt_test[outliers==0]
  
  dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]
  dt_test <- dt_test[!is.na(dt_test$numeric_value)] # drop NA
  
  # merge dt_test, 糖尿病, outcome by ID 
  dt_outcome <- fread(paste0(target_folder_path,outcome_file))
  
  # 定義 exclude columns
  dt_outcome <- dt_outcome[apply(dt_outcome[, ..exclude_columns], 1, sum) < 1]
  dt_test_T <- merge(dt_outcome, dt_test, by = "ID", all.x = TRUE) 
  dt_test_T <- dt_test_T[!is.na(Test_date)]
  
  # select valid data & data summary
  select_col <- c("ID", "Index_date", "Test_date")
  d_tmp <- dt_test_T[,..select_col]
  d_tmp <- d_tmp[, followup:= as.numeric(Test_date-Index_date) ]
  status_followup <- data.table(unique(d_tmp[["ID"]]))
  setnames(status_followup, "V1", "ID")
  
  num_interval <- 5
  tracking_interval <- 90
  event_interval <- 45
  for (m in 0:(num_interval-1)) {
    lower <- m * tracking_interval - event_interval + 1
    upper <- m * tracking_interval + event_interval
    d_tmp[, event := as.integer(followup >= lower & followup <= upper)]
    dt <- d_tmp[, .(event = as.integer( any(event == 1)) ), by = ID] 
    #cat("coverage_ratio:", (sum(dt$event)/nrow(dt)),"\n" ) 
    setnames(dt, "event", paste0("m_", m))
    status_followup <- merge(status_followup, dt, by = "ID")
  }
  
  # valid ID: by rowsum = num_interval
  test_dist_n <- status_followup[,-1]
  row_sum <- rowSums(test_dist_n)
  valid_ID <- status_followup[row_sum==num_interval]$ID
  
  csv_file_name <- paste0(target_folder_path, t,"_valid_ID.csv")
  fwrite(as.data.table(valid_ID), file = csv_file_name, row.names = FALSE)

  # get valid id data  
  dt_valid_test <- dt_test_T[ID %in% valid_ID]
  # valid data: 
  select_col <- c("ID", "Index_date", "Test_date", "numeric_value","unit")
  dt_valid_test <- dt_valid_test[,..select_col]
  dt_valid_test <- dt_valid_test[, followup := as.numeric(Test_date-Index_date)]
  
  dt_valid_test[, interval := create_intervals(dt_valid_test$followup, 
                                                  num_interval, tracking_interval,
                                                  event_interval)]
  dt_valid_test <- dt_valid_test[!is.na(interval)] 
  
  # cal mean, median , sd by ID, season
  result <- dt_valid_test[, .(
    mean_value = mean(numeric_value, na.rm = TRUE),
    median_value = median(numeric_value, na.rm = TRUE),
    sd_value = sd(numeric_value, na.rm = TRUE),
    n = .N
  ), by = .(ID, interval)]
  
  result_wide <- dcast(result, ID ~ interval, 
                       value.var = c("mean_value", "median_value", "sd_value", 
                                     "n"))
  result_wide[, total := rowSums((.SD),na.rm = TRUE), 
              .SDcols = paste0("n_", 0:(num_interval-1))]
  
  tb2_need_col <- c("median_value_0","median_value_1","median_value_2",
                    "median_value_3","median_value_4","n_0","n_1","n_2","n_3",
                    "n_4","total")
  
  tb2 <- create.table1(result_wide, need.col = tb2_need_col)
  csv_file_name <- paste0(target_folder_path, t, "_table2.csv")
  fwrite(tb2, file = csv_file_name, row.names = FALSE)
}
#===============================================================================
# create table3
# input: get valid id data  
tb3_T <- data.table()
for (t in names(Test_item)) {
  valid_id_name <- paste0(t,"_valid_ID.csv")
  dt_valid_id <- fread(paste0(target_folder_path, valid_id_name))
  
  tb3 <- data.table()
  for (f in outcome) {
    # 6 outcomes
    outcome_file <- paste0(f,"_clean.csv")
    dt <- fread(paste0(target_folder_path,outcome_file))
    dt[, exclude_valid_ID := ifelse(ID %in% dt_valid_id$valid_ID, 0, 1)]
    
    # begin exclude
    exclude_columns2 <- c(exclude_columns, "exclude_outcome1", 
                          "exclude_outcome2", "exclude_valid_ID")
    dt <- dt[apply(dt[, ..exclude_columns2], 1, sum) < 1]
    Total_people <- length(unique(dt$ID))
    need_col <- c("ID", names(dt)[28:29])
    dt <- dt[,..need_col]
    col_sums <- colSums(dt[,-1])
    
    tb3_r <- data.table(outcome_test = paste0(f,"_",t), t(col_sums), 
                        N = Total_people)
    setnames(tb3_r, c("outcome_test","# of event", "sum of follow up", "N"))
    tb3 <- rbind(tb3, tb3_r)
  }
  tb3_T <- rbind(tb3_T,tb3)
}
csv_file_name <- paste0(target_folder_path, "table3_all_outcome_summary.csv")
fwrite(tb3_T, file = csv_file_name, row.names = FALSE)


#===============================================================================
# create table 4~15 
file_name <- paste0(target_folder_path,"lab_result_swt.csv")
dt_lab <- fread(file_name)
for (t in names(Test_item)) {
  valid_id_name <- paste0(t,"_valid_ID.csv")
  dt_valid_id <- fread(paste0(target_folder_path, valid_id_name))
  Test_ID <- Test_item[[t]]$ID
  unit_p <- Test_item[[t]]$unit
  
  # select test_item
  dt_test <- dt_lab[grepl(paste0("^", paste(Test_ID, collapse="|^")), 
                          Test_item)]
  # clean test values
  dt_test <- dt_test[, `:=`(clean_value = str_replace_all(VALUE, unit_p, ""), 
                            unit = unit_p)]
  dt_test <- dt_test[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
  dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]

  for (f in outcome) {
    outcome_file <- paste0(f,"_clean.csv")
    dt <- fread(paste0(target_folder_path,outcome_file))
    dt[, exclude_valid_ID := ifelse(ID %in% dt_valid_id$valid_ID, 0, 1)]
    dt_test_T <- merge(dt, dt_test, by = "ID", all.x = TRUE) 
    csv_file_name <- paste0(target_folder_path, t,"_", f,".csv") 
    fwrite(dt_test_T, file = csv_file_name, row.names = FALSE)
  }
}


