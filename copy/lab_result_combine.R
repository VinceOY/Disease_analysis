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
# Test_ID: 兩院R_ITEM疾病碼
# Test_ID_w: 萬芳疾病碼
# unit: 檢驗項目排除單位
parameters <- list(
  outcome = "PeripheralVascDisease", 
  Test_item = "ALBUMIN",
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
                       "ConjunctivaDis"),
  Test_ID = list(HbA1c = c("014701","F09006B"), 
                 ALBUMIN = c("010301","11D101","F09038C")),
  unit = list(HbA1c = c("%"), ALBUMIN = c("(?i) g/dl"))
)

outcome <- parameters$outcome
outcome_file <- paste0(outcome,"_clean.csv")
exclude_columns <- parameters$exclude_columns

target_folder_path <- parameters$target_folder_path
related_diseases <- parameters$related_diseases
target_disease <- related_diseases[1]
outcome_diseases <- related_diseases[2:7]

Test_item <- parameters$Test_item
Test_ID <- parameters$Test_ID$ALBUMIN
unit_p <- parameters$unit$ALBUMIN
#===============================================================================
# 檢驗結果: LAB result # OITEM unique RITEM
# 兩院結果合併、select column, rename
result_files <- c("v_labresult_t.csv", "v_labresult_s.csv")
d_result <- data.table()
for (file in result_files) {
  d_tmp <- fread(paste0(target_folder_path, file))
  d_result <- rbind(d_result, d_tmp)
}
names(d_result)
d_result <- d_result[,c("CHR_NO","P_DATE","R_ITEM","VALUE"), 
                     with = FALSE]
d_result <- standardized_date(d_result, "P_DATE")
setnames(d_result, "CHR_NO", "ID")
setnames(d_result, "P_DATE", "Test_date")
setnames(d_result, "R_ITEM", "Test_item")

#===============================================================================
# 檢驗結果2: v_exper_sign_w.csv 萬芳
# 萬芳結果select column, rename

result_files_w <- c("v_exper_sign_w.csv")
d_result_w <- fread(paste0(target_folder_path, result_files_w))
d_result_w <- d_result_w[,c("CHR_NO", "EXPER_DATE","GROUP_CODE","EXPER_DATA2"), 
                         with = FALSE]
d_result_w <- standardized_date(d_result_w, "EXPER_DATE")
setnames(d_result_w,"CHR_NO","ID")
setnames(d_result_w, "EXPER_DATE", "Test_date")
setnames(d_result_w, "GROUP_CODE", "Test_item")
setnames(d_result_w, "EXPER_DATA2", "VALUE")

#===============================================================================
# merge: lab results(s, w, t) and output csv
lab_result <- rbind(d_result, d_result_w)
csv_file_name <- paste0(target_folder_path,"lab_result_swt.csv")
fwrite(lab_result, file = csv_file_name, row.names = FALSE)

#===============================================================================
# 從lab_result挑選出目標疾病
lab_result <- fread(paste0(target_folder_path,"lab_result_swt.csv"))

dt_test <- lab_result[grepl(paste0("^", paste(Test_ID, collapse="|^")), 
                            Test_item)]

csv_file_name <- paste0(target_folder_path,"ALBUMIN_result_swt.csv")
fwrite(dt_test, file = csv_file_name, row.names = FALSE)

#===============================================================================
# clean data
dt_test <- fread(paste0(target_folder_path,"HbA1c_result_swt.csv"))
dt_test <- dt_test[, `:=`(clean_value = str_replace_all(VALUE, unit_p, ""), 
                          unit = unit_p)]
dt_test <- dt_test[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat(" # of outliers:",nrow(dt_test[dt_test[,outliers==1]]))
dt_test <- dt_test[outliers==0]

dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]
dt_test <- dt_test[!is.na(dt_test$numeric_value)] # drop NA
summary(dt_test) 

#===============================================================================
# merge dt_test, 糖尿病, outcome by ID 
dt_outcome <- fread(paste0(target_folder_path,outcome_file))
# 定義 exclude columns
dt_outcome <- dt_outcome[apply(dt_outcome[, ..exclude_columns], 1, sum) < 1]
dt_test_T <- merge(dt_outcome, dt_test, by = "ID", all.x = TRUE) 
dt_test_T <- dt_test_T[!is.na(Test_date)]

#===============================================================================
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
  cat("coverage_ratio:", (sum(dt$event)/nrow(dt)),"\n" ) 
  setnames(dt, "event", paste0("m_", m))
  status_followup <- merge(status_followup, dt, by = "ID")
}

# valid ID: by rowsum = num_interval
test_dist_n <- status_followup[,-1]
row_sum <- rowSums(test_dist_n)
valid_ID <- status_followup[row_sum==num_interval]$ID

csv_file_name <- paste0(target_folder_path,Test_item,"_valid_ID.csv")
fwrite(as.data.table(valid_ID), file = csv_file_name, row.names = FALSE)

#===============================================================================
# get valid id data
create_intervals <- function(values, num_interval, tracking_interval, event_interval) {
  intervals <- rep(NA, length(values))
  for (m in 0:(num_interval-1)) {
    lower <- m * tracking_interval - event_interval + 1
    upper <- m * tracking_interval + event_interval
    intervals[values >= lower & values <= upper] <- paste(m)
  }
  return(intervals)
}

dt_test_valid_dt <- dt_test_T[ID %in% valid_ID]
cat(" # of data:", nrow(dt_test_valid_dt), "\n", "# of ID:", 
    length(unique(dt_test_valid_dt$ID)))

# valid data: 
select_col <- c("ID", "Index_date", "Test_date", "numeric_value","unit")
dt_test_valid_dt <- dt_test_valid_dt[,..select_col]
dt_test_valid_dt <- dt_test_valid_dt[, followup:= 
                                       as.numeric(Test_date-Index_date)]


dt_test_valid_dt[, interval := create_intervals(dt_test_valid_dt$followup, 
                                                 num_interval, tracking_interval,
                                                 event_interval)]
dt_test_valid_dt <- dt_test_valid_dt[!is.na(intervals)]

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
names(result_wide)

#===============================================================================
# table1: outcome: eye / exclude basic / age / index date
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
summary(dt_outcome)
tb1 <- create.table1(dt_outcome, 
                     need.col = c(continuous_col,category_col))
print(tb1)
csv_file_name <- paste0(target_folder_path,"table1_basic.csv") # excel 
fwrite(tb1, file = csv_file_name, row.names = FALSE)

#===============================================================================
# table2: outcome follow up summary
names(result_wide)
tb2_need_col <- c("median_value_0","median_value_1","median_value_2",
                  "median_value_3","median_value_4","n_0","n_1","n_2","n_3",
                  "n_4","total")
tb2 <- create.table1(result_wide, need.col = tb2_need_col)
print(tb2)
csv_file_name <- paste0(target_folder_path, outcome,"_", 
                        Test_item, "_table2123.csv")
fwrite(tb2, file = csv_file_name, row.names = FALSE)

#===============================================================================
# clean values
dt_test <- dt_test[, `:=`(clean_value = str_replace_all(VALUE, unit_p, ""), 
                          unit = unit_p)]
dt_test <- dt_test[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat(" # of outliers:",nrow(dt_test[dt_test[,outliers==1]]))

dt_test <- dt_test[dt_test[,outliers==0]]
cat(" # of data:", nrow(dt_test), "\n", "# of ID:", length(unique(dt_test$ID)))

dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]
dt_test[is.na(dt_test$numeric_value)]
dt_test <- dt_test[!is.na(dt_test$numeric_value)]
summary(dt_test) 

dt_test[, R_ITEM := Test_item]
setnames(dt_test, "R_ITEM", "Test_item")
setnames(dt_test, "P_DATE", "Test_date")
head(dt_test)


#===============================================================================
# merge: diabetes people,  lab_result
dt_outcome <- fread(paste0(target_folder_path,outcome_file))

# 定義 exclude columns
dt_outcome <- dt_outcome[apply(dt_outcome[, ..exclude_columns], 1, sum) < 1]
dt_diabete <- merge(dt_outcome, d_result, by = "ID", all.x = TRUE) 
unique(dt_diabete[!is.na(Test_date)]$ID) # N: 糖尿病患者
# 



#===============================================================================
# 檢驗代號: EXP ITEM 
item_files <- c("v_exp_item_t.csv", "v_exp_item_s.csv")
d_item <- data.table()
for (file in item_files) {
  d_tmp <- fread(paste0(target_folder_path, file))
  d_item <- rbind(d_item, d_tmp)
}
d_item <- d_item[,c("O_ITEM", "R_ITEM","R_ITEM_NAME"), with = FALSE]


#===============================================================================
# Test_item
dt_test <- dt_diabete[grepl(paste0("^", paste(Test_ID, collapse="|^")), 
                            R_ITEM)]

cat(" # of data:", nrow(dt_test), "\n", "# of ID:", length(unique(dt_test$ID)))
# clean values
dt_test <- dt_test[, `:=`(clean_value = str_replace_all(VALUE, unit_p, ""), 
                          unit = unit_p)]
dt_test <- dt_test[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat(" # of outliers:",nrow(dt_test[dt_test[,outliers==1]]))

dt_test <- dt_test[dt_test[,outliers==0]]
cat(" # of data:", nrow(dt_test), "\n", "# of ID:", length(unique(dt_test$ID)))

dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]
dt_test[is.na(dt_test$numeric_value)]
dt_test <- dt_test[!is.na(dt_test$numeric_value)]
summary(dt_test) 

dt_test[, R_ITEM := Test_item]
setnames(dt_test, "R_ITEM", "Test_item")
setnames(dt_test, "P_DATE", "Test_date")
head(dt_test)

#===============================================================================
## 檢驗結果2: v_exper_sign_w.csv 萬芳
result_files_w <- c("v_exper_sign_w.csv")
d_result_w <- fread(paste0(target_folder_path, result_files_w))
d_result_w <- d_result_w[,c("CHR_NO", "EXPER_DATE","GROUP_CODE","EXPER_DATA2"), 
                         with = FALSE]
d_result_w <- standardized_date(d_result_w, "EXPER_DATE")
setnames(d_result_w,"CHR_NO","ID")
head(d_result_w)

#===============================================================================
# merge id, item name, count: w
dt_diabete_w <- merge(dt_outcome, d_result_w, by = "ID", all.x = TRUE) 

#===============================================================================
# select disease:w
dt_test_w <- dt_diabete_w[dt_diabete_w$GROUP_CODE == Test_ID_w]

cat(" # of data:", nrow(dt_test_w), "\n", "# of ID:", 
    length(unique(dt_test_w$ID)))
# clean values
dt_test_w <- dt_test_w[, `:=`(clean_value = str_replace_all(EXPER_DATA2, unit_p, ""), 
                              unit = unit_p)]
dt_test_w <- dt_test_w[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat(" # of outliers:",nrow(dt_test_w[dt_test_w[,outliers==1]]))

dt_test_w <- dt_test_w[dt_test_w[,outliers==0]]
cat(" # of data:", nrow(dt_test_w), "\n", "# of ID:", 
    length(unique(dt_test_w$ID)))

dt_test_w <- dt_test_w[, numeric_value := as.numeric(clean_value)]
dt_test_w[is.na(dt_test_w$numeric_value)]
dt_test_w <- dt_test_w[!is.na(dt_test_w$numeric_value)]
summary(dt_test_w) 

dt_test_w[, GROUP_CODE := Test_item]
setnames(dt_test_w, "GROUP_CODE", "Test_item")
setnames(dt_test_w, "EXPER_DATE", "Test_date")
setnames(dt_test_w, "EXPER_DATA2", "VALUE")
names(dt_test_w) == names(dt_test)

#===============================================================================
# merge two dts => HbA1c
head(dt_test)
head(dt_test_w)
dt_test_T <- rbind(dt_test, dt_test_w)
length(unique(dt_test_T$ID))
nrow(dt_test_T)

#===============================================================================
# select valid data & data summary
select_col <- c("ID", "SEX_TYPE", "Index_date", paste0(outcome,"_event"),
                "Test_date")
d_tmp <- dt_test_T[,..select_col]
d_tmp <- d_tmp[, followup:= as.numeric(Test_date-as.Date(Index_date)) ]
status_followup <- data.table(unique(d_tmp[["ID"]]))
setnames(status_followup, "V1", "ID")


i <- seq(0, 360, 90)
num_interval <- 5
tracking_interval <- 90
event_interval <- 45
for (m in 0:(num_interval-1)) {
  lower <- m * tracking_interval - event_interval + 1
  upper <- m * tracking_interval + event_interval
  
  d_tmp[, event := as.integer(followup >= lower & followup <= upper)]
  
  dt <- d_tmp[, .(event = as.integer( any(event == 1)) ), by = ID] # by id 看發生與否
  
  cat("coverage_ratio:", (sum(dt$event)/nrow(dt)),"\n" ) 
  setnames(dt, "event", paste0("m_", m))
  status_followup <- merge(status_followup, dt, by = "ID")
}

# valid ID: by rowsum = num_interval
test_dist_n <- status_followup[,-1]
row_sum <- rowSums(test_dist_n)
valid_ID <- status_followup[row_sum==num_interval]$ID

#===============================================================================
dt_test_valid_dt <- dt_test_T[ID %in% valid_ID]
cat(" # of data:", nrow(dt_test_valid_dt), "\n", "# of ID:", 
    length(unique(dt_test_valid_dt$ID)))
names(dt_test_valid_dt)
# valid data: 
select_col <- c("ID", "Index_date", "Test_date", "numeric_value","unit")
dt_test_valid_dt <- dt_test_valid_dt[,..select_col]
dt_test_valid_dt <- dt_test_valid_dt[, followup:= 
                                       as.numeric(Test_date-as.Date(Index_date))]

calculate_interval <- function(followup) {
  if (followup >= -45 & followup <= 45) {
    return(0)
  } else if (followup >= 46 & followup <= 135) {
    return(1)
  } else if (followup >= 136 & followup <= 225) {
    return(2)
  } else if (followup >= 226 & followup <= 315) {
    return(3)
  } else if (followup >= 316 & followup <= 405) {
    return(4)
  } else {
    return(NA)
  }
}

dt_test_valid_dt[, interval := sapply(followup, calculate_interval)]
dt_test_valid_dt <- dt_test_valid_dt[!is.na(interval)]
cat(" # of data:", nrow(dt_test_valid_dt), "\n", "# of ID:", 
    length(unique(dt_test_valid_dt$ID)))

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
names(result_wide)

# check data 
test_item_n <- result_wide[,17:21]
row_sum <- rowSums(test_item_n)
result_wide[row_sum < 5]
csv_file_name <- paste0(target_folder_path,as.character(tracking_interval), 
                        "_interval_summary.csv")
#fwrite(result_wide, file = csv_file_name, row.names = FALSE)
na_counts <- sapply(result_wide, function(x) sum(is.na(x)))
na_counts

#===============================================================================
# table1: outcome: eye / exclude basic / age / index date: all data 
dt_outcome[,year:= substr(dt_outcome[["Index_date"]], 1, 4)] # year 
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
summary(dt_outcome)
tb1 <- create.table1(dt_outcome, 
                     need.col = c(continuous_col,category_col))
print(tb1)
csv_file_name <- paste0(target_folder_path,"table1_basic.csv") # excel 
fwrite(tb1, file = csv_file_name, row.names = FALSE)

#===============================================================================
# table2: outcome follow up summary: # of test items
tb2_need_col <- c("median_value_0","median_value_1","median_value_2",
                  "median_value_3","median_value_4","n_0","n_1","n_2","n_3",
                  "n_4","total")
tb2 <- create.table1(result_wide, need.col = tb2_need_col)
print(tb2)
csv_file_name <- paste0(target_folder_path, outcome,"_", 
                        Test_item, "_table2.csv")
fwrite(tb2, file = csv_file_name, row.names = FALSE)

#===============================================================================
# table3: # of test item and outcomes 
data <- c()

# lab
ex_1.1
ex_1.2


for(d in c()){
  by d 
  ex1
  ex1
  dt[, exclude_valid_ID := ifelse(ID %in% valid_ID, 0, 1)]
  fwrite
  
}

dt <- fread(paste0(target_folder_path,outcome_file))
dt[, exclude_valid_ID := ifelse(ID %in% valid_ID, 0, 1)]
cat("# of invalid ID:", length(unique(dt[dt[,exclude_Indexdate==0&
                                             exclude_ID==0&
                                             exclude_AGE==0& 
                                             exclude_valid_ID==1]]$ID)),"\n")
cat("# of valid ID:", length(unique(dt[dt[, exclude_valid_ID==0]]$ID)),"\n")


csv_file_name <- paste0(target_folder_path, outcome,"_", 
                        Test_item, "_dtf.csv")
fwrite(dt, file = csv_file_name, row.names = FALSE)
cat("# of invalid outcome1:",length(unique(dt[dt[, exclude_Indexdate==0&
                                                   exclude_ID==0&
                                                   exclude_AGE==0&
                                                   exclude_outcome1==1&
                                                   exclude_outcome2==0]]$ID)),"\n")

cat("# of invalid outcome2:",length(unique(dt[dt[, exclude_Indexdate==0&
                                                   exclude_ID==0&
                                                   exclude_AGE==0&
                                                   exclude_outcome1==0&
                                                   exclude_outcome2==1]]$ID)),"\n")
exclude_columns2 <- c(exclude_columns, "exclude_outcome1", "exclude_outcome2")
dt <- dt[apply(dt[, ..exclude_columns2], 1, sum) < 1]
cat("# of invalid ID:", length(unique(dt[dt[, exclude_valid_ID==1]]$ID)),"\n")
cat("# of valid ID:", length(unique(dt[dt[, exclude_valid_ID==0]]$ID)),"\n")

dt <- dt[ID %in% valid_ID]
need_col <- c("ID", names(dt)[28:29])
dt <- dt[,..need_col]
col_sums <- colSums(dt[,-1])
tb4_r <- data.table(t(col_sums))
setnames(tb4_r, c("# of event", "sum of follow up"))
tb4_r

