rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/standard_function.R")
library(data.table)
library(ggplot2)
library(stringr)
#===============================================================================
## get disease code list
folder_path2 = "C:/Users/USER/Downloads/TMUCRD_2021_csv/"
dt1 <- fread(paste0(folder_path2, "ICD92ICD10.csv"))
dt1 <- dt1[, c("ICD-9-CM代碼" ,"ICD-10-CM"),with = FALSE]
setnames(dt1, "ICD-9-CM代碼", "ICD9")
setnames(dt1, "ICD-10-CM", "ICD10")
ICD9_codes = list(EyeComp = c("362.01","362.02","362.55", "362.11",
                              "365.44","369","361","369.60"),
                  CardioDisease = c("414.9","413.9","411.1","412",
                                    "428","425","402"),
                  CerebroDisease = c("435.9","434.91","432.9",
                                     "437.1"),
                  PeripheralVascDisease = c("440.2","440.20","440.21",
                                            "440.22","440.23","440.24"),
                  Nephropathy = c("583", "585", "V451"),
                  DiabeticNeuro = c("357.2","337.1","353"),
                  Hypertension = "401", PeripheralEnthe = "726",
                  UnknownCauses = "799", LipoidMetabDis = "272", 
                  AcuteURI = "465", AbdPelvicSymptoms = c("724","789"),
                  Dermatophytosis = "110", GenSymptoms = "780",
                  RespChestSymptoms = "786", HeadNeckSymptoms = "784",
                  ContactDermEczema = "692", ViralInfection = "79",
                  ObesityHyperal = "278", JointDisorders = "719",
                  AcuteBronchitis = "466", SoftTissueDis = "729",
                  BloodExamFindings = "790", RefractionDis = "367",
                  ConjunctivaDis = "372"
)

disease_code_list = list()
for (a in names(ICD9_codes)) {
  d_tmp <- dt1[grepl(paste0("^", paste(ICD9_codes[[a]], collapse="|^")), ICD9)]
  d_tmp[,disease_name := a]
  disease_code_list[[a]] <- c(unique(d_tmp$ICD9),unique(d_tmp$ICD10))
}

ICD10_codes_diabete = c("E08","E09","E10","E11","E12")
d_tmp <- dt1[grepl(paste0("^", paste(ICD10_codes_diabete, 
                                     collapse="|^")), ICD10)]
d_tmp[,disease_name := "diabete"]
disease_code_list[["diabete"]] <- c(unique(d_tmp$ICD9), unique(d_tmp$ICD10))
#ICD9_codes_Stroke = c("430","431","432","433","434","435","436")
#ICD10_codes_Stroke = c("I60","I61","I62","I63","I64","I65","I66","I67","I68",
#                       "I69")
#disease_code_list[["Stroke"]] <- c(ICD9_codes_Stroke, ICD10_codes_Stroke)

#===============================================================================
# set parameters
parameters <- list(
  basic_files = c("v_chr_basic_w.csv","v_chr_basic_t.csv","v_chr_basic_s.csv"),
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
                       "ConjunctivaDis"),
  disease_codes = disease_code_list,
  data_sets = list(
    opd = list(
      file_list = c("v_opd_basic_w.csv","v_opd_basic_t.csv",
                    "v_opd_basic_s.csv"),
      disease_ID_cols = c("ICD9_CODE1", "ICD9_CODE2", "ICD9_CODE3", 
                          "ICD10_CODE1", "ICD10_CODE2", "ICD10_CODE3", 
                          "ICD10_CODE4", "ICD10_CODE5"),
      id_col = "CHR_NO",
      date_col = "OPD_DATE",
      k = 3
    ),
    ipd = list(
      file_list = c("v_ipd_basic_w.csv","v_ipd_basic_t.csv",
                    "v_ipd_basic_s.csv"),
      disease_ID_cols = c("ICD10_CODE1", "ICD10_CODE2", "ICD10_CODE3", 
                          "ICD10_CODE4", "ICD10_CODE5", "ICD10_CODE6", 
                          "ICD10_CODE7"),
      id_col = "CHR_NO",
      date_col = "IPD_DATE",
      k = 1
    )
  )
)

basic_files <- parameters$basic_files
folder_path <- parameters$ori_folder_path
target_folder_path <- parameters$target_folder_path
disease_codes <- parameters$disease_codes
related_diseases <- parameters$related_diseases
target_disease <- parameters$target_disease
outcome_diseases <- related_diseases[1:6]
control_diseases <- related_diseases[7:length(related_diseases)]
taget_outcome_diseases <- c(target_disease, outcome_diseases)


#===============================================================================
## 檢驗結果: LAB result # 缺萬芳 # other files
result_files <- c("v_labresult_t.csv", "v_labresult_s.csv")
d_result <- data.table()
for (file in result_files) {
  d_tmp <- fread(paste0(folder_path, file))
  d_result <- rbind(d_result, d_tmp)
}
d_result <- d_result[,c("CHR_NO","B_DATE","R_ITEM","VALUE"), with = FALSE]
setnames(d_result, "CHR_NO", "ID")
d_result <- standardized_date(d_result, "B_DATE")

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
# merge id, item name, count 
dt_diabete <- fread(paste0(target_folder_path,"EyeComp_clean.csv"))
length(unique(dt_diabete$ID))
# 定義 exclude columns
exclude_columns <- c("exclude_AGE", "exclude_ID", "exclude_Indexdate",
                     "exclude_outcome1","exclude_outcome2")


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
# example: ALBUMIN
ALBUMIN_dt <- d_item[grep("ALBUMIN", R_ITEM_NAME, ignore.case = TRUE)]
ALBUMIN_ID <- c("010301","11D101")
ALBUMIN <- dt_diabete[grepl(paste0("^", paste(ALBUMIN_ID, collapse="|^")), R_ITEM)]
ALBUMIN <- ALBUMIN[, clean_value := str_replace_all(VALUE, "(?i) g/dl", "")]
ALBUMIN <- ALBUMIN[, clean_value := str_replace_all(clean_value, "<", "")] # drop
ALBUMIN <- ALBUMIN[, numeric_value := as.numeric(clean_value)]
ALBUMIN[is.na(ALBUMIN$numeric_value)]# check na
summary(ALBUMIN)

# Create density plot using ggplot2
ggplot(ALBUMIN, aes(x = numeric_value)) +
  geom_density() +
  ggtitle("ALBUMIN Test Density Plot") +
  labs(x = "ALBUMIN_values", y = "Density", title = "Density Plot")

#===============================================================================
# HbA1c
HbA1c_dt <- d_item[grep("HbA1c", R_ITEM_NAME, ignore.case = TRUE)]
HbA1c_ID <- "014701"
HbA1c <- dt_diabete[grep(HbA1c_ID, R_ITEM, ignore.case = TRUE)]
print(paste0("# of data: ",nrow(HbA1c)))
print(paste0("# of ID: ",length(unique(HbA1c$ID))))
# clean data
HbA1c <- HbA1c[, clean_value := str_replace_all(VALUE, "%", "")] 
HbA1c <- HbA1c[, unit := "%"] 

# exclude outliers: < > 
HbA1c <- HbA1c[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
print(paste0("# of outliers: ",nrow(HbA1c[HbA1c[,outliers==1]])))


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

# clean data
HbA1c <- HbA1c[HbA1c[,not_enough_data==0&outliers==0&invalid_date==0]]
print(paste0("# of data: ",nrow(HbA1c)))
print(paste0("# of ID: ",length(unique(HbA1c$ID))))

HbA1c <- HbA1c[, numeric_value := as.numeric(clean_value)]
HbA1c[is.na(HbA1c$numeric_value)]# check na
summary(HbA1c) 

# follow up date:
HbA1c <- HbA1c[,followup:= B_DATE-as.Date(Index_date)]


# Create density plot using ggplot2
ggplot(HbA1c, aes(x = numeric_value)) +
  geom_density() +
  ggtitle("HbA1c Test Density Plot") + 
  labs(x = "HbA1c_values", y = "Density", title = "Density Plot")

A <- c("R_ITEM", "VALUE")
csv_file_name <- paste0(target_folder_path, "HbA1c.csv")
fwrite(d_t[,..A], file = csv_file_name, row.names = FALSE)

#===============================================================================
# transfer to %
# remove not enough test data
# cal HAb1c variability
ALBUMIN_Diabete_ID <- length(unique(ALBUMIN$ID))
HbA1c_Diabete_ID <- length(unique(HbA1c$ID))
print(paste0("no HbA1c test data: ",total_ID - ALBUMIN_Diabete_ID))
print(paste0("no HbA1c test data: ",total_ID - HbA1c_Diabete_ID))

dt1 <- HbA1c[ID %in% unique(HbA1c$ID)[2]]
dt1 <- dt1[,c("Index_date","B_DATE")]
dt1 <- dt1[order(B_DATE)]
dt1[,followup := B_DATE-as.Date(Index_date)]

follow_up <- data.table()
for (i in 1:HbA1c_Diabete_ID) {
  dt1 <- HbA1c[ID %in% unique(HbA1c$ID)[i]]
  dt1 <- dt1[,c("ID","Index_date","B_DATE")]
  dt1 <- dt1[order(B_DATE)]
  a <- dt1[,followup := B_DATE-as.Date(Index_date)]
  print(a$followup)
  follow_up <- rbind(follow_up, a)
}


max_followup <- max(follow_up$followup)
i <- 120
follow_up[, follow_up_group := cut(follow_up, breaks = seq(0, max_followup, by = i), right = FALSE, 
                            labels = paste(seq(0, max_followup-i, by = i), 
                                           seq(i, max_followup, by = i), sep = "-"))]
# 計算每120天的間隔
interval_days <- 120
start_date <- min(follow_up$followup)
end_date <- max(data$date)
break_points <- seq(start_date, end_date, by = interval_days)

# 根據間隔分組
data$group <- cut(data$date, breaks = break_points, include.lowest = TRUE)



x1 <- data.table()
for (i in 1:num_ID) {
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
