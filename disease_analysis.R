rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/standard_function.R")
library(data.table)
library(ggplot2)
library(stringr)
#===============================================================================
# set parameters
parameters <- list(
  ori_folder_path = "C:/Users/USER/Downloads/TMUCRD_2021_csv/",
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

folder_path <- parameters$ori_folder_path
target_folder_path <- parameters$target_folder_path
related_diseases <- parameters$related_diseases
target_disease <- parameters$target_disease
outcome_diseases <- related_diseases[1:6]
control_diseases <- related_diseases[7:length(related_diseases)]
taget_outcome_diseases <- c(target_disease, outcome_diseases)

#===============================================================================
## 檢驗結果: LAB result # 缺萬芳 # other files # check P_DATE
result_files <- c("v_labresult_t.csv", "v_labresult_s.csv")
d_result <- data.table()
for (file in result_files) {
  d_tmp <- fread(paste0(folder_path, file))
  d_result <- rbind(d_result, d_tmp)
}
names(d_result)
d_result <- d_result[,c("CHR_NO","P_DATE", "B_DATE","O_ITEM","R_ITEM","VALUE"), 
                     with = FALSE]
d_result[d_result$O_ITEM=="0147",unique(R_ITEM)]
setnames(d_result, "CHR_NO", "ID")
d_result <- standardized_date(d_result, "B_DATE")
d_result <- standardized_date(d_result, "P_DATE")
d_result[, test_diff:= as.numeric(as.Date(B_DATE) - as.Date(P_DATE))]
quantile(d_result$test_diff,0.8)

#===============================================================================
# 檢驗代號: EXP ITEM # 缺萬芳
item_files <- c("v_exp_item_t.csv", "v_exp_item_s.csv")
d_item <- data.table()
for (file in item_files) {
  d_tmp <- fread(paste0(folder_path, file))
  d_item <- rbind(d_item, d_tmp)
}
d_item <- d_item[,c("R_ITEM","R_ITEM_NAME"), with = FALSE]

#===============================================================================
## 檢驗結果2: v_exper_sign_w.csv
result_files2 <- c("v_exper_sign_w.csv")
d_result2 <- fread(paste0(folder_path, result_files2))
d_result2 <- d_result2[,c("CHR_NO", "EXPER_DATE","GROUP_CODE","EXPER_DATA",
                          "EXPER_DATA2","EXPER_DATA3","EXPER_DATA4",
                          "EXPER_DATA5"), 
                       with = FALSE]
d_result2 <- standardized_date(d_result2, "EXPER_DATE")
csv_file_name <- paste0(target_folder_path,"renew_result.csv")
fwrite(d_result2, file = csv_file_name, row.names = FALSE)
setnames(d_result2,"CHR_NO","ID")
d_result2[d_result2$GROUP_CODE =="F09015C"]
#===============================================================================
# select disease
d_result2[d_result2$GROUP_CODE =="F09025C"]
HbA1c_w <- d_result2[d_result2$GROUP_CODE =="F09006B"]

HbA1c_w <- HbA1c_w[, `:=`(clean_value = str_replace_all(EXPER_DATA2, "%", ""), 
                          unit = "%")]
# exclude outliers: < > 
HbA1c_w <- HbA1c_w[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
cat("# of outliers: ",nrow(HbA1c_w[HbA1c_w[,outliers==1]]))
HbA1c_w <- HbA1c_w[HbA1c_w[,outliers==0]]
cat(" # of data:", nrow(HbA1c_w), "\n", "# of ID:", length(unique(HbA1c_w$ID)))

HbA1c_w <- HbA1c_w[, numeric_value := as.numeric(clean_value)]
HbA1c_w <- HbA1c_w[!is.na(HbA1c_w$numeric_value)]
HbA1c_w[,R_ITEM := "014701"]
names(HbA1c_w)
# merge id, item name, count 
dt_diabete <- fread(paste0(target_folder_path,"EyeComp_clean.csv"))
exclude_columns <- c("exclude_AGE", "exclude_ID", "exclude_Indexdate")

# exclude
dt_diabete <- dt_diabete[apply(dt_diabete[, ..exclude_columns], 1, sum) < 1]
dt_diabete <- merge(dt_diabete, HbA1c_w, by = "ID", all.x = TRUE) 
names(dt_diabete)
#===============================================================================
# select valid data & data summary
select_col <- c("ID", "SEX_TYPE", "Index_date", "EXPER_DATE")
d_tmp <- dt_diabete[,..select_col]
d_tmp <- d_tmp[, followup:= as.numeric(EXPER_DATE-as.Date(Index_date)) ]
d_tmp <- d_tmp[!is.na(followup)]
status_followup <- data.table(unique(d_tmp[["ID"]]))
setnames(status_followup, "V1", "ID")
num_interval <- 5
tracking_interval <- 90
event_interval <- 45
for (m in 0:(num_interval-1)) {
  lower <- m * tracking_interval - event_interval
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

cat(" # of data:", nrow(dt_diabete), "\n", "# of ID:", 
    length(unique(dt_diabete$ID)))

HbA1c_valid_dt <- dt_diabete[ID %in% valid_ID]
cat(" # of data:", nrow(HbA1c_valid_dt), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt$ID)))

select_col <- c("ID", "Index_date", "EXPER_DATE", "numeric_value","unit")
HbA1c_valid_dt <- HbA1c_valid_dt[,..select_col]
HbA1c_valid_dt <- HbA1c_valid_dt[, followup:= as.numeric(EXPER_DATE-as.Date(Index_date)) ]

HbA1c_valid_dt[, interval := floor(followup / tracking_interval) + 1]
HbA1c_valid_dt <- HbA1c_valid_dt[interval <= num_interval & interval >= 0]

cat(" # of data:", nrow(HbA1c_valid_dt), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt$ID)))

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
            .SDcols = paste0("n_", 0:num_interval)]

csv_file_name <- paste0(target_folder_path,as.character(tracking_interval), 
                        "_interval_summary_w.csv")
fwrite(result_wide, file = csv_file_name, row.names = FALSE)

na_counts <- sapply(result_wide, function(x) sum(is.na(x)))
na_counts
print(result_wide)




#===============================================================================
# merge id, item name, count 
dt_diabete <- fread(paste0(target_folder_path,"EyeComp_clean.csv"))
length(unique(dt_diabete$ID))
# 定義 exclude columns
exclude_columns <- c("exclude_AGE", "exclude_ID", "exclude_Indexdate")
# exclude
dt_diabete <- dt_diabete[apply(dt_diabete[, ..exclude_columns], 1, sum) < 1]
dt_diabete <- merge(dt_diabete, d_result, by = "ID", all.x = TRUE) 
total_ID <- length(unique(dt_diabete$ID))

dt_count <- as.data.table(table(dt_diabete$R_ITEM))
setnames(dt_count, "V1", "R_ITEM")
dt_count <- merge(d_item, dt_count, by = "R_ITEM", all.y = TRUE)
dt_count <- dt_count[order(-N)]
dt_count <- dt_count[!duplicated(dt_count)]
csv_file_name <- paste0(target_folder_path, "test_items.csv")
fwrite(dt_count, file = csv_file_name, row.names = FALSE)

#===============================================================================
# ex1: ALBUMIN
ALBUMIN_dt <- d_item[grep("ALBUMIN", R_ITEM_NAME, ignore.case = TRUE)]
ALBUMIN_ID <- c("010301","11D101")
ALBUMIN <- dt_diabete[grepl(paste0("^", paste(ALBUMIN_ID, collapse="|^")), 
                            R_ITEM)]
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

# follow up date:
ALBUMIN <- ALBUMIN[,followup:= B_DATE-as.Date(Index_date)]
followup <- as.numeric(ALBUMIN$followup)

# time interval
breaks <- seq(floor(min(followup)), ceiling(max(followup)), by = 30)
# hist plot 
p <- ggplot(data = data.frame(days = followup), aes(x = days)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Follow-ups with 30-Day Intervals", x = "Days", 
       y = "Frequency") + 
  theme_minimal()
print(p)

# save plot
ALBUMIN_png_name <- paste0(target_folder_path, "ALBUMIN_follow.png")
ggsave(ALBUMIN_png_name, plot = p, width = 8, height = 6, dpi = 300, 
       bg = "white")
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

HbA1c <- HbA1c[, numeric_value := as.numeric(clean_value)]
HbA1c[is.na(HbA1c$numeric_value)]
summary(HbA1c) 
na_counts1 <- sapply(HbA1c, function(x) sum(is.na(x)))

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
# select valid data & data summary
select_col <- c("ID", "SEX_TYPE", "Index_date", "P_DATE", "B_DATE")
d_tmp <- HbA1c[,..select_col]
d_tmp <- d_tmp[, followup:= as.numeric(P_DATE-as.Date(Index_date)) ]

status_followup <- data.table(unique(d_tmp[["ID"]]))
setnames(status_followup, "V1", "ID")
num_interval <- 5
tracking_interval <- 90
event_interval <- 45
for (m in 0:(num_interval-1)) {
  lower <- m * tracking_interval - event_interval
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
valid_ID

HbA1c_valid_dt <- HbA1c[ID %in% valid_ID]
cat(" # of data:", nrow(HbA1c_valid_dt), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt$ID)))

select_col <- c("ID", "Index_date", "P_DATE", "numeric_value","unit")
HbA1c_valid_dt <- HbA1c_valid_dt[,..select_col]
HbA1c_valid_dt <- HbA1c_valid_dt[, followup:= as.numeric(P_DATE-as.Date(Index_date)) ]

HbA1c_valid_dt[, interval := floor(followup / tracking_interval) + 1]
HbA1c_valid_dt <- HbA1c_valid_dt[interval <= num_interval & interval >= 0]
cat(" # of data:", nrow(HbA1c_valid_dt), "\n", "# of ID:", 
    length(unique(HbA1c_valid_dt$ID)))

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
            .SDcols = paste0("n_", 0:num_interval)]



csv_file_name <- paste0(target_folder_path,as.character(tracking_interval), 
                        "_interval_summary.csv")
fwrite(result_wide, file = csv_file_name, row.names = FALSE)

na_counts <- sapply(result_wide, function(x) sum(is.na(x)))
na_counts
print(result_wide)

#===============================================================================
for (col in names(test_dist_numeric)) {
  hist_data <- test_dist_numeric[[col]]
  p <-   ggplot(data.frame(x = hist_data), aes(x)) +
          geom_histogram(binwidth = 1, fill = "blue", color = "black") +
          labs(title = paste("Histogram of", col),
            x = col, y = "Frequency")
  print(p)
}

csv_file_name <- paste0(target_folder_path, "test_distribution_s.csv")
fwrite(test_dist, file = csv_file_name, row.names = FALSE)


#===============================================================================
# Check values distribution
ggplot(HbA1c, aes(x = numeric_value)) +
  geom_density() +
  ggtitle("HbA1c Test Density Plot") + 
  labs(x = "HbA1c_values", y = "Density", title = "Density Plot")

# follow up date:
HbA1c <- HbA1c[,followup:= B_DATE-as.Date(Index_date)]
followup <- as.numeric(HbA1c$followup)


# time interval 
breaks <- seq(floor(min(followup)), ceiling(max(followup)), by = 30)

# hist 
p <- ggplot(data = data.frame(days = followup), aes(x = days)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Follow-ups with 30-Day Intervals", x = "Days", 
       y = "Frequency") + 
  theme_minimal()
print(p)

# save plot 
HbA1c_png_name <- paste0(target_folder_path, "HbA1c_follow.png")
ggsave(HbA1c_png_name, plot = p, width = 8, height = 6, dpi = 300, 
       bg = "white")


