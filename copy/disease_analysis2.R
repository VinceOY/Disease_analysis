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
  target_folder_path = "C:/Users/USER/Downloads/disease_df/",
  target_disease = "Diabete",
  related_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro",
                       "Hypertension","PeripheralEnthe","UnknownCauses",
                       "LipoidMetabDis","AcuteURI","AbdPelvicSymptoms",
                       "Dermatophytosis","GenSymptoms","RespChestSymptoms",
                       "HeadNeckSymptoms","ContactDermEczema","ViralInfection",
                       "ObesityHyperal", "JointDisorders", "AcuteBronchitis", 
                       "SoftTissueDis", "BloodExamFindings", "RefractionDis",
                       "ConjunctivaDis")
)

target_folder_path <- parameters$target_folder_path
related_diseases <- parameters$related_diseases
target_disease <- parameters$target_disease
outcome_diseases <- related_diseases[1:6]
control_diseases <- related_diseases[7:length(related_diseases)]
taget_outcome_diseases <- c(target_disease, outcome_diseases)

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

# check O_ITEM是否unique R_ITEM
#d_result[d_result$O_ITEM=="0147",unique(R_ITEM)]

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
# merge id, item name, count 
dt_eye <- fread(paste0(target_folder_path,"EyeComp_clean.csv"))
length(dt_eye$ID)

# 定義 exclude columns
exclude_columns <- c("exclude_AGE", "exclude_ID", "exclude_Indexdate")
dt_eye <- dt_eye[apply(dt_eye[, ..exclude_columns], 1, sum) < 1]
dt_diabete <- merge(dt_eye, d_result, by = "ID", all.x = TRUE) 
names(d_result)

#===============================================================================
# ex2:HbA1c
HbA1c_dt <- d_item[grep("HbA1c", R_ITEM_NAME, ignore.case = TRUE)]
HbA1c_ID <- unique(HbA1c_dt[["R_ITEM"]])
HbA1c <- dt_diabete[grep(HbA1c_ID, R_ITEM, ignore.case = TRUE)]
cat(" # of data:", nrow(HbA1c), "\n", "# of ID:", length(unique(HbA1c$ID)))
# clean values
HbA1c <- HbA1c[, `:=`(clean_value = str_replace_all(VALUE, "%", ""), 
                      unit = "%")]
# exclude outliers: < > 
HbA1c <- HbA1c[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat("# of outliers: ",nrow(HbA1c[HbA1c[,outliers==1]]))
HbA1c <- HbA1c[HbA1c[,outliers==0]]
cat(" # of data:", nrow(HbA1c), "\n", "# of ID:", length(unique(HbA1c$ID)))

# transfer to numeric & check na
HbA1c <- HbA1c[, numeric_value := as.numeric(clean_value)]
HbA1c[is.na(HbA1c$numeric_value)]
summary(HbA1c) 
HbA1c[, R_ITEM := "HbA1c"]
setnames(HbA1c, "R_ITEM", "Test_item")
setnames(HbA1c, "P_DATE", "Test_date")
#===============================================================================
## 檢驗結果2: v_exper_sign_w.csv 萬芳
result_files_w <- c("v_exper_sign_w.csv")
d_result_w <- fread(paste0(target_folder_path, result_files_w))
d_result_w <- d_result_w[,c("CHR_NO", "EXPER_DATE","GROUP_CODE","EXPER_DATA2"), 
                         with = FALSE]
d_result_w <- standardized_date(d_result_w, "EXPER_DATE")
setnames(d_result_w,"CHR_NO","ID")
head(d_result_w,10)
#===============================================================================
# merge id, item name, count: w
exclude_columns <- c("exclude_AGE", "exclude_ID", "exclude_Indexdate")
dt_eye <- dt_eye[apply(dt_eye[, ..exclude_columns], 1, sum) < 1]
dt_diabete_w <- merge(dt_eye, d_result_w, by = "ID", all.x = TRUE) 

#===============================================================================
# select disease:w
HbA1c_w <- dt_diabete_w[dt_diabete_w$GROUP_CODE =="F09006B"]
HbA1c_w <- HbA1c_w[, `:=`(clean_value = str_replace_all(EXPER_DATA2, "%", ""), 
                          unit = "%")]
# exclude outliers w: < > 
HbA1c_w <- HbA1c_w[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat("# of outliers: ",nrow(HbA1c_w[HbA1c_w[,outliers==1]]))
HbA1c_w <- HbA1c_w[HbA1c_w[,outliers==0]]
cat(" # of data:", nrow(HbA1c_w), "\n", "# of ID:", length(unique(HbA1c_w$ID)))
HbA1c_w <- HbA1c_w[, numeric_value := as.numeric(clean_value)]
HbA1c_w[is.na(HbA1c_w$numeric_value)]
HbA1c_w <- HbA1c_w[!is.na(HbA1c_w$numeric_value)]

names(HbA1c_w)
HbA1c_w[, GROUP_CODE := "HbA1c"]
setnames(HbA1c_w, "GROUP_CODE", "Test_item")
setnames(HbA1c_w, "EXPER_DATE", "Test_date")
setnames(HbA1c_w, "EXPER_DATA2", "VALUE")
#===============================================================================
# merge two dts 
HbA1c_T <- rbind(HbA1c, HbA1c_w)
length(unique(HbA1c_T$ID))
nrow(HbA1c_T)
#===============================================================================
# select valid data & data summary
select_col <- c("ID", "SEX_TYPE", "Index_date", "EyeComp_event","Test_date")
d_tmp <- HbA1c_T[,..select_col]
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

HbA1c_valid_dt <- HbA1c_T[ID %in% valid_ID]
cat(" # of data:", nrow(HbA1c_valid_dt), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt$ID)))
names(HbA1c_valid_dt)
# valid data: 
select_col <- c("ID", "Index_date", "Test_date", "numeric_value","unit")
HbA1c_valid_dt <- HbA1c_valid_dt[,..select_col]
HbA1c_valid_dt <- HbA1c_valid_dt[, followup:= 
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

HbA1c_valid_dt[, interval := sapply(followup, calculate_interval)]
HbA1c_valid_dt <- HbA1c_valid_dt[!is.na(interval)]
cat(" # of data:", nrow(HbA1c_valid_dt), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt$ID)))

# cal mean, median , sd by ID, season
result <- HbA1c_valid_dt[, .(
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
test_item <- result_wide[,17:21]
row_sum <- rowSums(test_item)
result_wide[row_sum < 5]
result_wide[is.na(result_wide$mean_value_4)]
csv_file_name <- paste0(target_folder_path,as.character(tracking_interval), 
                        "_interval_summary.csv")
fwrite(result_wide, file = csv_file_name, row.names = FALSE)
na_counts <- sapply(result_wide, function(x) sum(is.na(x)))
na_counts

#===============================================================================
# create table:
# table1: outcome: eye / exclude basic / age / index date
dt_eye[,year:= substr(dt_eye[["Index_date"]], 1, 4)]
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

dt_eye[, (category_col) := lapply(.SD, as.factor), .SDcols = category_col]
summary(dt_eye)
tb1 <- create.table1(dt_eye, 
                     need.col = c(continuous_col,category_col))
print(tb1)
csv_file_name <- paste0(target_folder_path,"table1_basic.csv")
fwrite(tb1, file = csv_file_name, row.names = FALSE)

# tb2 / tb3
names(result_wide)
tb2_need_col <- c("median_value_0","median_value_1","median_value_2",
                  "median_value_3","median_value_4","n_0","n_1","n_2","n_3",
                  "n_4","total")
tb2 <- create.table1(result_wide, need.col = tb2_need_col)
print(tb2)
csv_file_name <- paste0(target_folder_path,"table2_HbA1c.csv")
fwrite(tb2, file = csv_file_name, row.names = FALSE)

# tb4
outcome_files <- list.files(target_folder_path, 
                            pattern = paste0("_clean"), 
                            full.names = TRUE, ignore.case = TRUE)
tb4_result <- data.table()
for (i in outcome_files) {
  dt <- fread(i)
  exclude_columns <- c("exclude_AGE", "exclude_ID", "exclude_Indexdate")
  dt <- dt[apply(dt[, ..exclude_columns], 1, sum) < 1]
  need_col <- c("ID", names(dt)[28:29])
  dt <- dt[,..need_col]
  dt <- dt[ID %in% valid_ID]
  col_sums <- colSums(dt[,-1]) 
  tb4_r <- data.table(t(col_sums))
  setnames(tb4_r, c("# of event", "sum of follow up"))
  tb4_result <- rbind(tb4_result, tb4_r)
}
print(tb4_result)

csv_file_name <- paste0(target_folder_path,"table4_HbA1c.csv")
fwrite(tb4_result, file = csv_file_name, row.names = FALSE)




#===============================================================================
## 檢驗結果2: v_exper_sign_w.csv 萬芳
result_files_w <- c("v_exper_sign_w.csv")
d_result_w <- fread(paste0(target_folder_path, result_files_w))
d_result_w <- d_result_w[,c("CHR_NO", "EXPER_DATE","GROUP_CODE","EXPER_DATA",
                            "EXPER_DATA2","EXPER_DATA3","EXPER_DATA4",
                            "EXPER_DATA5"), with = FALSE]
d_result_w <- standardized_date(d_result_w, "EXPER_DATE")
setnames(d_result_w,"CHR_NO","ID")
head(d_result_w,10)

#===============================================================================
# merge id, item name, count: w
exclude_columns <- c("exclude_AGE", "exclude_ID", "exclude_Indexdate")
dt_eye <- dt_eye[apply(dt_eye[, ..exclude_columns], 1, sum) < 1]
dt_diabete_w <- merge(dt_eye, d_result_w, by = "ID", all.x = TRUE) 

#===============================================================================
# select disease:w
HbA1c_w <- dt_diabete_w[dt_diabete_w$GROUP_CODE =="F09006B"]
HbA1c_w <- HbA1c_w[, `:=`(clean_value = str_replace_all(EXPER_DATA2, "%", ""), 
                          unit = "%")]
# exclude outliers w: < > 
HbA1c_w <- HbA1c_w[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat("# of outliers: ",nrow(HbA1c_w[HbA1c_w[,outliers==1]]))
HbA1c_w <- HbA1c_w[HbA1c_w[,outliers==0]]
cat(" # of data:", nrow(HbA1c_w), "\n", "# of ID:", length(unique(HbA1c_w$ID)))
HbA1c_w <- HbA1c_w[, numeric_value := as.numeric(clean_value)]
HbA1c_w[is.na(HbA1c_w$numeric_value)]
HbA1c_w <- HbA1c_w[!is.na(HbA1c_w$numeric_value)]
names(HbA1c_w)

#===============================================================================
# select valid data & data summary: W
select_col <- c("ID", "SEX_TYPE", "Index_date", "EXPER_DATE")
d_tmp <- HbA1c_w[,..select_col]
d_tmp <- d_tmp[, followup:= as.numeric(EXPER_DATE-as.Date(Index_date)) ]
d_tmp <- d_tmp[!is.na(followup)]
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
cat(" # of data:", nrow(HbA1c_w), "\n", "# of ID:", 
    length(unique(HbA1c_w$ID)))
test_dist_n <- status_followup[,-1]
row_sum <- rowSums(test_dist_n)
valid_ID_w <- status_followup[row_sum==num_interval]$ID
HbA1c_valid_dt_w <- HbA1c_w[ID %in% valid_ID_w]
cat(" # of data:", nrow(HbA1c_valid_dt_w), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt_w$ID)))

# valid data: follow time < 450, follow time > -90
select_col <- c("ID", "Index_date", "EXPER_DATE", "numeric_value","unit")
HbA1c_valid_dt_w <- HbA1c_valid_dt_w[,..select_col]
HbA1c_valid_dt_w <- HbA1c_valid_dt_w[, followup:= 
                                       as.numeric(EXPER_DATE-as.Date(Index_date)) ]

HbA1c_valid_dt_w[, interval := sapply(followup, calculate_interval)]
HbA1c_valid_dt_w <- HbA1c_valid_dt_w[!is.na(interval)]
cat(" # of data:", nrow(HbA1c_valid_dt_w), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt_w$ID)))

# cal mean, median , sd by ID, season
result <- HbA1c_valid_dt_w[, .(
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

csv_file_name <- paste0(target_folder_path,as.character(tracking_interval), 
                        "_interval_summary_w.csv")
fwrite(result_wide, file = csv_file_name, row.names = FALSE)
na_counts <- sapply(result_wide, function(x) sum(is.na(x)))
na_counts

print(result_wide)
names(HbA1c_w)
names(HbA1c)
#===============================================================================
# merge w,(s,t):
# need columns 

#===============================================================================
# Check values distribution
ggplot(HbA1c_w, aes(x = numeric_value)) +
  geom_density() +
  ggtitle("HbA1c Test Density Plot") + 
  labs(x = "HbA1c_w_values", y = "Density", title = "Density Plot")

#===============================================================================
# ex1: ALBUMIN
ALBUMIN_dt <- d_item[grep("AST", R_ITEM_NAME, ignore.case = TRUE)]
for (i in 1:length(unique(ALBUMIN_dt$O_ITEM))) {
  print(ALBUMIN_dt[O_ITEM==unique(ALBUMIN_dt$O_ITEM)[i]])
}
ALBUMIN_ID <- c("0125")
ALBUMIN <- dt_diabete[grepl(paste0("^", paste(ALBUMIN_ID, collapse="|^")), 
                            O_ITEM)]

csv_file_name <- paste0(target_folder_path,"Creatinine.csv")
fwrite(ALBUMIN, file = csv_file_name, row.names = FALSE)

fwrite(ALBUMIN,)
#ALBUMIN_ID <- c("010301","11D101")
#ALBUMIN <- dt_diabete[grepl(paste0("^", paste(ALBUMIN_ID, collapse="|^")), 
#                            R_ITEM)]

# check O_ITEM是否unique R_ITEM
d_result[d_result$O_ITEM=="0103",unique(R_ITEM)]


cat(" # of data:", nrow(ALBUMIN), "\n", "# of ID:", length(unique(ALBUMIN$ID)))
# clean values
ALBUMIN <- ALBUMIN[, `:=`(clean_value = str_replace_all(VALUE,"(?i) g/dl", ""), 
                          unit = "g/dl")]
ALBUMIN <- ALBUMIN[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat(" # of outliers:",nrow(ALBUMIN[ALBUMIN[,outliers==1]]))

ALBUMIN <- ALBUMIN[ALBUMIN[,outliers==0]]
cat(" # of data:", nrow(ALBUMIN), "\n", "# of ID:", length(unique(ALBUMIN$ID)))

ALBUMIN <- ALBUMIN[, numeric_value := as.numeric(clean_value)]
ALBUMIN[is.na(ALBUMIN$numeric_value)]
summary(ALBUMIN) 

# Check values distribution
ggplot(ALBUMIN, aes(x = numeric_value)) +
  geom_density() +
  ggtitle("ALBUMIN Test Density Plot") + 
  labs(x = "ALBUMIN_values", y = "Density", title = "Density Plot")

#===============================================================================
# exclude 檢測日 < 確診日
HbA1c[, invalid_date := ifelse((B_DATE < as.Date(Index_date)), 1, 0)]
print(paste0("# of invalid data: ",nrow(HbA1c[HbA1c[,invalid_date==1]])))

# 計算每個ID的出現次數
ID_counts <- table(HbA1c$ID)
# 找出出現次數<=3次的ID
valid_IDs <- names(ID_counts[ID_counts <= 3])
# 過濾出現次數<=3次的資料
HbA1c <- HbA1c[, not_enough_data:= ifelse(HbA1c$ID %in% valid_IDs, 1, 0) ]
print(paste0("# of not_enough_data: ",nrow(HbA1c[HbA1c[,not_enough_data==1]])))
print(paste0("# of not_enough_data_ID: ",
             length(unique(HbA1c[HbA1c[,not_enough_data==1]]$ID))))

#===============================================================================
# check number of test item 
dt_count <- as.data.table(table(dt_diabete$R_ITEM))
setnames(dt_count, "V1", "R_ITEM")
dt_count <- merge(d_item, dt_count, by = "R_ITEM", all.y = TRUE)
dt_count <- dt_count[order(-N)]
dt_count <- dt_count[!duplicated(dt_count)]
csv_file_name <- paste0(target_folder_path, "test_items.csv")
fwrite(dt_count, file = csv_file_name, row.names = FALSE)

#===============================================================================
for (col in names(test_dist_numeric)) {
  hist_data <- test_dist_numeric[[col]]
  p <-   ggplot(data.frame(x = hist_data), aes(x)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = paste("Histogram of", col),
         x = col, y = "Frequency")
  print(p)
}

