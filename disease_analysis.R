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
names(d_result2)

d_result2[d_result2$GROUP_CODE =="F09006B"]




d_result2 <- d_result2[,c("CHR_NO", "EXPER_NO","COMP_DATE","EXPER_DATA",
                          "EXPER_DATA2","EXPER_DATA3","EXPER_DATA4",
                          "EXPER_DATA5","ITEM_NO"), with = FALSE]


d_it <- fread(paste0(folder_path, "v_exper_referdata_w.csv"))

d_r <- fread(paste0(folder_path, "v_labresult_bound_t.csv"))
names(d_r) # FEE_



head(d_result2)
d_result2[!is.na(d_result2$REPORT)]


d_result2 <- d_result2[,c("CHR_NO", "CONF_DATE",
                          "EXPER_NO","ITEM_NO", "REPORT"), with = FALSE]
setnames(d_result2, "CHR_NO", "ID")

## 檢驗結果2:  EXPERIMENT  
d1 <- fread(paste0(folder_path, "v_experiment_t.csv"))
d1 <- d1[,c("ITEM_NO", "ITEM_NAME"), with = FALSE]
HbA1c_d1 <- d1[grep("HbA1c", ITEM_NAME, ignore.case = TRUE)]

HbA1cd2 <- d_result2[grep(HbA1c_d1$ITEM_NO, ITEM_NO, ignore.case = TRUE)]
HbA1cd2[!is.na(HbA1cd2$REPORT)]



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
# aggregate: 檢測情況
# check different time interval 
select_col <- c("ID", "SEX_TYPE", "Index_date", "P_DATE", "B_DATE")
d_tmp <- HbA1c[,..select_col]
#d_tmp <- d_tmp[, followup:= as.numeric(P_DATE-as.Date(Index_date)) ]
d_tmp <- d_tmp[, followup:= as.numeric(B_DATE-as.Date(Index_date)) ]
test_dist <- data.table(unique(d_tmp[["ID"]]))
setnames(test_dist, "V1", "ID")
unit_time <- 4
index_interval <- 90
event_interval <- 45

for (m in 0:unit_time) {
  lower <- m * index_interval - event_interval
  upper <- m * index_interval + event_interval
  d_tmp[, event := as.integer(followup >= lower & followup <= upper)]
  event_status <- c
  # by id count 
  for (i in unique(d_tmp$ID)) {
    dt <- d_tmp[ID %in% i]
    event_status <- c(event_status, ifelse(sum(dt$event) >= 1, 1, 0))
  }
  dt2 <- data.table(ID = unique(d_tmp$ID), event = event_status)
  cat("coverage_ratio:", (sum(dt2$event)/nrow(dt2)),"\n" ) # check
  setnames(dt2, "event", paste0("m_", m))
  test_dist <- merge(test_dist, dt2, by = "ID")
}

test_dist_n <- test_dist[,-1]
row_sum <- rowSums(test_dist_n)
test_dist_n[row_sum==5]



# go back check data 
a <- test_dist[test_dist$m_0==0]$ID
d_tmp[ID %in% a[3]]

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


#===============================================================================
# transfer to %
# remove not enough test data
# cal HAb1c variability
ALBUMIN_Diabete_ID <- length(unique(ALBUMIN$ID))
HbA1c_Diabete_ID <- length(unique(HbA1c$ID))
print(paste0("no HbA1c test data: ",total_ID - ALBUMIN_Diabete_ID))
print(paste0("no HbA1c test data: ",total_ID - HbA1c_Diabete_ID))

x1 <- data.table()
for (i in 1:HbA1c_Diabete_ID) {
  dt1 <- HbA1c[ID %in% unique(HbA1c$ID)[i]]
  
  # 過濾 value 欄位 
  dt1 <- dt1[, numeric_value := numeric_value / 100]
  
  # sort values by (B_DATE)
  dt1 <- dt1[order(B_DATE)]
  
  # 計算每個數值與前一筆之間的差異 # warning here: shift()
  dt1 <- dt1[, diff := numeric_value - shift(numeric_value, type = "lag")]
  
  # 判斷差異是否大於 0.005 (即 0.5%)
  dt1 <- dt1[, diff_gt_0.5pct := abs(diff) > 0.005]
  
  # 計算 HVS 的分子部分 (大於 0.5% 的次數)
  HVS_numerator <- sum(dt1[["diff_gt_0.5pct"]], na.rm = TRUE)
  
  # 計算 HVS (分子部分除以總個數)
  HVS <- (HVS_numerator / nrow(dt1)) 
  
  # 計算均值 (mean)
  mean_value <- mean(dt1$numeric_value, na.rm = TRUE)
  
  # 計算標準差 (SD)
  sd_value <- sd(dt1$numeric_value, na.rm = TRUE)
  
  # 計算均方根 (RMS)
  rms_value <- sqrt(mean(dt1$numeric_value^2, na.rm = TRUE))
  
  # 計算變異係數 (CV)
  cv_value <- sd_value / mean_value
  
  # 每一個病人
  x <- data.table(
    "ID" = unique(dt1$ID),
    "SEX" = unique(dt1$SEX_TYPE),
    "mean" = mean_value,
    "SD" = sd_value,
    "RMS" = rms_value,
    "CV" = cv_value,
    "HVS" = HVS
  )
  x1 <- rbind(x1,x)
}

summary(x1)
