rm(list=ls());gc()
source("tool_function/standard_function.R")
source("tool_function/tableone.R")
library(data.table)
library(stringr)
#===============================================================================
# set parameter
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/step3/",
  output_path = "C:/Users/USER/Downloads/proj_data/step4/",
  Test_item = list(HbA1c = list(ID = c("014701","F09006B"), unit = c("%")),
                   ALBUMIN = list(ID = c("010301","11D101","F09038C"), 
                                  unit = c("(?i) g/dl"))), 
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
)
input_path <- parameters$input_path
output_path <- parameters$output_path
Test_item <- parameters$Test_item
outcome_diseases <- parameters$outcome_diseases

#===============================================================================
# create table1
dt_outcome <- fread(paste0(input_path, "dt_exclude1.csv"))
dt_outcome[,year:= year(Index_date)] 
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
csv_file_name <- paste0(output_path,"table1_basic.csv") # excel 
fwrite(tb1, file = csv_file_name, row.names = FALSE)

#===============================================================================
# create table2:
# input: lab_result_swt.csv, dt_exclude1
create_intervals <- function(values, season_i, interval) {
  intervals <- rep(NA, length(values))
  unit <- season_i[2]-season_i[1]
  for (i in season_i) {
    lower <- i - interval + 1
    upper <- i + interval
    intervals[values >= lower & values <= upper] <- paste(i/unit)
  }
  return(intervals)
}

file_name <- paste0(input_path,"lab_result_swt.csv")
dt_lab <- fread(file_name)
season_i <- seq(0,365,90)
interval <- 45

# find disease lab + add interval_col
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

  dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]
  dt_test <- dt_test[, na_col := ifelse(is.na(numeric_value), 1, 0)]
  
  dt_outcome <- fread(paste0(input_path,"dt_exclude1.csv"))
  dt_test_T <- merge(dt_outcome, dt_test, by = "ID", all.x = TRUE) # 得糖尿病沒檢驗的人
  dt_test_T <- dt_test_T[, exclude_testdate_na := ifelse(is.na(Test_date), 1, 0)]

  # exclude
  #dt_test_T <- dt_test_T[outliers==0]
  #dt_test_T <- dt_test_T[na_col==0]
  #dt_test_T <- dt_test_T[exclude_testdate_na==0]
  
  dt_test_T <- dt_test_T[, followup := as.numeric(Test_date-Index_date)]
  dt_test_T[, interval := create_intervals(dt_test_T$followup, season_i, interval)]
  
  length(unique(dt_test_T$ID))
  
  csv_file_name <- paste0(output_path,t,"_lab.csv")  
  fwrite(dt_test_T, file = csv_file_name, row.names = FALSE)
} 


# get valid id data  
for (t in names(Test_item)) {
  dt_test_T <- fread(paste0(output_path, t, "_lab.csv"))
  dt_test_T <- dt_test_T[!is.na(interval)] 
  
  # get valid ID
  valid_ID <- dt_test_T[, .N, by = .(ID, interval)][, .N, by = ID
                                                    ][N == length(season_i), ID]
  csv_file_name <- paste0(output_path, t, "_valid_ID.csv")
  fwrite(data.table(ID = valid_ID), file = csv_file_name, row.names = FALSE)
  
  dt_valid_test <- dt_test_T[ID %in% valid_ID]
  
  dt_valid_test <- dt_valid_test[, .(
    mean_value = mean(numeric_value, na.rm = TRUE),
    median_value = median(numeric_value, na.rm = TRUE),
    sd_value = sd(numeric_value, na.rm = TRUE),
    n = .N
  ), by = .(ID, interval)]
  
  result_wide <- dcast(dt_valid_test, ID ~ interval, 
                       value.var = c("mean_value", "median_value", "sd_value", 
                                     "n"))
  result_wide[, total := rowSums((.SD),na.rm = TRUE), 
              .SDcols = paste0("n_", 0:(length(season_i)-1))]
  # table2
  tb2_need_col <- c("median_value_0","median_value_1","median_value_2",
                    "median_value_3","median_value_4","n_0","n_1","n_2","n_3",
                    "n_4","total")
  tb2 <- create.table1(result_wide, need.col = tb2_need_col)
  csv_file_name <- paste0(output_path, t, "_table2.csv")
  fwrite(tb2, file = csv_file_name, row.names = FALSE)
}

#===============================================================================
# create table3
# input: get valid id data  
tb3_T <- data.table()
for (t in names(Test_item)) {
  dt_test <- fread(paste0(output_path, t, "_lab.csv"))
  valid_id_name <- paste0(t,"_valid_ID.csv")
  valid_id <- fread(paste0(output_path, valid_id_name))
  tb3 <- data.table()
  for (o in outcome_diseases) {
    dt <- fread( paste0(input_path, o,"_exclude2.csv"))
    dt <- dt[ID %in% valid_id$ID]
    valid_id_f <- dt$ID
    fwrite(data.table(ID = valid_id_f), paste0(output_path, o, "_", t, 
                                         "_valid_ID.csv"), row.names = FALSE)
    ### out putdf
    clean_df <- dt_test[ID %in% valid_id_f]
    csv_file_name <- paste0(output_path, t,"_", o,"_dtf.csv") 
    fwrite(clean_df, file = csv_file_name, row.names = FALSE)
    
    #table3
    Total_people <- length(unique(dt$ID))
    need_col <- c("ID", paste0(o,"_event"), paste0(o,"_followup")) 
    dt <- dt[,..need_col]
    tb3_r <- data.table(outcome_test = paste0(o,"_",t), t(colSums(dt[,-1])), 
                        N = Total_people)
    setnames(tb3_r, c("outcome_test","# of event", "sum of follow up", "N"))
    tb3 <- rbind(tb3, tb3_r)
  }
  tb3_T <- rbind(tb3_T,tb3)
}
csv_file_name <- paste0(output_path, "table3_all_outcome_summary.csv")
fwrite(tb3_T, file = csv_file_name, row.names = FALSE)

