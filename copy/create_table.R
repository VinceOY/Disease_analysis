rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/standard_function.R")
source("tool_function/tableone.R")
library(data.table)
library(ggplot2)
library(stringr)
#===============================================================================
# set parameters
# define parameters: 
# outcome: Diabetes related outcome, 
# Test_item: 檢驗項目
# exclude_columns: 清除age, ID, index_date
# target_folder_path: 檔案存放路徑
# related_diseases: 目標疾病, outcome, control, 
# unit: 檢驗項目排除單位
# test item 還有一圈
parameters <- list(
  Test_item = list(HbA1c = c("%"),
                   ALBUMIN = c("(?i) g/dl")),
  exclude_columns = c("exclude_AGE", "exclude_ID", "exclude_Indexdate"),
  target_folder_path = "C:/Users/USER/Downloads/disease_df/",
  related_diseases = c("Diabete","EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro",
                       "Hypertension","PeripheralEnthe","UnknownCauses",
                       "LipoidMetabDis","AcuteURI","AbdPelvicSymptoms",
                       "Dermatophytosis","GenSymptoms","RespChestSymptoms",
                       "HeadNeckSymptoms","ContactDermEczema","ViralInfection",
                       "ObesityHyperal", "JointDisorders", "AcuteBronchitis", 
                       "SoftTissueDis", "BloodExamFindings", "RefractionDis",
                       "ConjunctivaDis")
)

related_diseases <- parameters$related_diseases
exclude_columns <- parameters$exclude_columns
target_folder_path <- parameters$target_folder_path
outcome <- related_diseases[2:7]

Test_item <- parameters$Test_item
tb4_T <- data.table()
for (t in names(Test_item)) {
  # 2 test items
  unit_p <- Test_item[[t]]
  test_file_name <- paste0(t,"_result_swt.csv")
  
  # load HbA1c table: drop ><、NA 
  dt_test <- fread(paste0(target_folder_path,test_file_name))
  dt_test <- dt_test[, `:=`(clean_value = str_replace_all(VALUE, unit_p, ""), 
                            unit = unit_p)]
  dt_test <- dt_test[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
  #cat(" # of outliers:",nrow(dt_test[dt_test[,outliers==1]]), "\n")
  dt_test <- dt_test[outliers==0]
  
  dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]
  #cat(" # of NA:",nrow(dt_test[is.na(dt_test$numeric_value)]), "\n")
  dt_test <- dt_test[!is.na(dt_test$numeric_value)] # drop NA
  
  tb4 <- data.table()
  for (f in outcome) {
    # 6 outcome
    outcome_file <- paste0(f,"_clean.csv")
    
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
    
    csv_file_name <- paste0(target_folder_path,t,"_valid_ID.csv")
    fwrite(as.data.table(valid_ID), file = csv_file_name, row.names = FALSE)

    #===========================================================================
    # table3: outcome follow up summary: need info: valid id 
    dt <- dt_outcome
    dt[, exclude_valid_ID := ifelse(ID %in% valid_ID, 0, 1)]
    #cat("# of people:", length(unique(dt$ID)),"\n")
    #cat("# of invalid ID:", length(unique(dt[dt[, exclude_valid_ID==1]]$ID)),"\n")
    #cat("# of invalid outcome1:",length(unique(dt[dt[, exclude_outcome1==1]]$ID)),"\n")
    #cat("# of invalid outcome2:",length(unique(dt[dt[, exclude_outcome2==1]]$ID)),"\n")
    
    # begin exclude
    exclude_columns2 <- c(exclude_columns, "exclude_outcome1", 
                          "exclude_outcome2", "exclude_valid_ID")
    dt <- dt[apply(dt[, ..exclude_columns2], 1, sum) < 1]
    Total_people <- length(unique(dt$ID))
    need_col <- c("ID", names(dt)[28:29])
    dt <- dt[,..need_col]
    col_sums <- colSums(dt[,-1])
    
    tb4_r <- data.table(outcome_test = paste0(f,"_",t),
                        t(col_sums), N = Total_people)
    setnames(tb4_r, c("outcome_test","# of event", "sum of follow up", "N"))
    tb4 <- rbind(tb4, tb4_r)
  }
  tb4_T <- rbind(tb4_T,tb4)
}

csv_file_name <- paste0(target_folder_path,"table4.csv")
fwrite(tb4_T,csv_file_name, row.names = FALSE )

#===============================================================================
# create table2:
# input : dt_test_T, valid_ID
create_intervals <- function(values, num_interval, tracking_interval, event_interval) {
  intervals <- rep(NA, length(values))
  for (m in 0:(num_interval-1)) {
    lower <- m * tracking_interval - event_interval + 1
    upper <- m * tracking_interval + event_interval
    intervals[values >= lower & values <= upper] <- paste(m)
  }
  return(intervals)
}

for (t in names(Test_item)) {
  t <-  names(Test_item)[1]
  unit_p <- Test_item[[t]]
  test_file_name <- paste0(t,"_result_swt.csv")
  
  # load Test_item table: drop ><、NA 
  dt_test <- fread(paste0(target_folder_path,test_file_name))
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

  # get valid id data  
  valid_id_name <- paste0(t,"_valid_ID.csv")
  valid_id <- fread(paste0(target_folder_path, valid_id_name))
  dt_test_valid_dt <- dt_test_T[ID %in% valid_ID]
  
  # valid data: 
  select_col <- c("ID", "Index_date", "Test_date", "numeric_value","unit")
  dt_test_valid_dt <- dt_test_valid_dt[,..select_col]
  dt_test_valid_dt <- dt_test_valid_dt[, followup:= 
                                         as.numeric(Test_date-Index_date)]
  
  dt_test_valid_dt[, interval := create_intervals(dt_test_valid_dt$followup, 
                                                  num_interval, tracking_interval,
                                                  event_interval)]
  dt_test_valid_dt <- dt_test_valid_dt[!is.na(interval)] 
  #####
  #output next step
  
  
  # cal mean, median , sd by ID, season
  result <- dt_test_valid_dt[, .(
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