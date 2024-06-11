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
parameters <- list(
  outcome = "PeripheralVascDisease", 
  Test_item = "HbA1c",
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
  Test_ID = list(HbA1c = c("014701"), ALBUMIN = c("010301","11D101")),
  Test_ID_w = list(HbA1c = c("F09006B"), ALBUMIN = c("F09038C")),
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
Test_ID <- parameters$Test_ID$HbA1c
Test_ID_w <- parameters$Test_ID_w$HbA1c
unit_p <- parameters$unit$HbA1c
#===============================================================================
## 檢驗結果: LAB result # OITEM unique RITEM
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

#===============================================================================
# 檢驗代號: EXP ITEM 
item_files <- c("v_exp_item_t.csv", "v_exp_item_s.csv")
d_item <- data.table()
for (file in item_files) {
  d_tmp <- fread(paste0(target_folder_path, file))
  d_item <- rbind(d_item, d_tmp)
}
d_item <- d_item[,c("O_ITEM", "R_ITEM","R_ITEM_NAME"), with = FALSE]

#HbA1c_dt <- d_item[grep("HbA1c", R_ITEM_NAME, ignore.case = TRUE)]
#HbA1c_ID <- unique(HbA1c_dt[["R_ITEM"]])
#===============================================================================
# merge id, item name, count 
dt_outcome <- fread(paste0(target_folder_path,outcome_file))
length(dt_outcome$ID)

# 定義 exclude columns
dt_outcome <- dt_outcome[apply(dt_outcome[, ..exclude_columns], 1, sum) < 1]
dt_diabete <- merge(dt_outcome, d_result, by = "ID", all.x = TRUE) 

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
# merge two dts 
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
num_interval <- 5
tracking_interval <- 90
event_interval <- 45
for (m in 0:(num_interval-1)) {
  lower <- m * tracking_interval - event_interval + 1
  upper <- m * tracking_interval + event_interval
  d_tmp[, event := as.integer(followup >= lower & followup <= upper)]
  dt <- d_tmp[, .(event = as.integer(any(event == 1))), by = ID]
  cat("coverage_ratio:", (sum(dt$event)/nrow(dt)),"\n" ) 
  setnames(dt, "event", paste0("m_", m))
  status_followup <- merge(status_followup, dt, by = "ID")
}

# valid ID: by rowsum = num_interval
test_dist_n <- status_followup[,-1]
row_sum <- rowSums(test_dist_n)
valid_ID <- status_followup[row_sum==num_interval]$ID

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
# table1: outcome: eye / exclude basic / age / index date
dt_outcome[,year:= substr(dt_outcome[["Index_date"]], 1, 4)]
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
csv_file_name <- paste0(target_folder_path,"table1_basic.csv")
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
                        Test_item, "_table2.csv")
fwrite(tb2, file = csv_file_name, row.names = FALSE)

#===============================================================================
# table4
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

