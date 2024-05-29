rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/fetch_function.R")
source("tool_function/standard_function.R")
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

#my_data_frame <- data.frame(disease_code_list$DiabeticNeuro)
#fwrite(my_data_frame, 
#       "C:/Users/USER/Downloads/disease_df/ICD_code.csv", row.names = FALSE)
#===============================================================================
## get data
parameters <- list(
  ori_folder_path = "C:/Users/USER/Downloads/TMUCRD_2021_csv/",
  target_folder_path = "C:/Users/USER/Downloads/disease_df/",
  target_disease = "Diabete_",
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

folder_path <- parameters$ori_folder_path
target_folder_path <- parameters$target_folder_path
disease_codes <- parameters$disease_codes
related_diseases <- parameters$related_diseases
target_disease <- parameters$target_disease

for (data_set_name in names(parameters$data_sets)) {
  data_set <- parameters$data_sets[[data_set_name]]
  dt_file_list <- data_set$file_list
  dt_id_col <- data_set$id_col
  dt_date_col <- data_set$date_col
  dt_target_ID_cols <- c(dt_id_col, dt_date_col)
  dt_disease_ID_cols <- data_set$disease_ID_cols
  
  for (file in dt_file_list) {
    hospital_names <- sub(".*_(.*)\\.csv", "\\1", file)
    d_tmp <- fread(paste0(folder_path, file))
    merge_df <- fetch_data2(d_tmp, dt_target_ID_cols, dt_disease_ID_cols, 
                            disease_codes)
    for (name in names(merge_df)) {
      csv_file_name <- paste0(target_folder_path, name, "_",
                              data_set_name, "_", hospital_names, ".csv")
      fwrite(merge_df[[name]], file = csv_file_name, row.names = FALSE)
      print(csv_file_name)
    }
  }
}

#===============================================================================
# dt_target: ID, Index_date 
# data source: opd/ipd_3院_diabete檔案6個
target_list <- list()
P_list <- list()

for (data_set_name in names(parameters$data_sets)) {
  dt_id_col <- parameters$data_sets[[data_set_name]]$id_col
  dt_date_col <- parameters$data_sets[[data_set_name]]$date_col
  dt_valid_times <- parameters$data_sets[[data_set_name]]$k
  target_files = list.files(target_folder_path, 
                            pattern = paste0(target_disease,data_set_name), 
                            full.names = TRUE, ignore.case = TRUE)
  dt_target <- data.table()
  for (file in target_files) {
    d_tmp <- fread(file)
    d_tmp <- standardized_date(d_tmp, dt_date_col)
    if(nrow(dt_target)==0){
      dt_target <- d_tmp
    }else{
      dt_target <- rbind(dt_target, d_tmp)
    }
    target_list[[data_set_name]] <- dt_target
    P_list <- append(P_list, list(list(df = target_list[[data_set_name]], 
                                       idcol = dt_id_col, datecol = dt_date_col, 
                                       k = dt_valid_times)))
  }
}
dt_target <- find_earliest_date(P_list)
setnames(dt_target , "Date", "Index_date")

#===============================================================================
# dt_basic: ["ID", "SEX_TYPE", "BIRTH_DATE", "DEATH_DATE" ]
# data source:  病人資料_CHR_basic
basic_files <- c("v_chr_basic_w.csv","v_chr_basic_t.csv","v_chr_basic_s.csv")
dt_basic <- data.table()
for (file in basic_files) {
  d_tmp <- fread(paste0(folder_path, file))
  dt_basic <- rbind(dt_basic, d_tmp)
}
dt_basic <- dt_basic[, c("CHR_NO","SEX_TYPE","BIRTH_DATE","DEATH_DATE"),
                     with = FALSE]
dt_basic[, unknown_ID := ifelse(.N > 1, 1, 0), by = CHR_NO]
cols_to_pad <- c("BIRTH_DATE", "DEATH_DATE")
dt_basic <- standardized_date(dt_basic, "BIRTH_DATE") 
dt_basic <- standardized_date(dt_basic, "DEATH_DATE") 
setnames(dt_basic, "CHR_NO", "ID")

#===============================================================================
# disease_list: [tb1,tb2,tb3,...]
# tb_disease = [ID, ("疾病","DATE")]
disease_list <- list()
for (d in related_diseases) {
  disease <- d
  disease_files <- list.files(target_folder_path, pattern = d, 
                              full.names = TRUE, ignore.case = TRUE)
  dt_c <- data.table()
  for (file in disease_files) {
    d_tmp <- fread(file)
    if(nrow(d_tmp)!=0){
      dt_date_col <- names(d_tmp)[2]
      d_tmp <- standardized_date(d_tmp, names(d_tmp)[2])
      setnames(d_tmp, dt_date_col, "DATE")
      dt_c <- rbind(dt_c, d_tmp)
    }
  }
  setnames(dt_c,"CHR_NO", "ID")
  disease_list[[disease]] <- dt_c
}

#===============================================================================
# Merge tables
# step1: merge basic, index_date by CHR_NO and cal age / sep age group 
dt_merge <- merge(dt_basic, dt_target, by = "ID", all = FALSE) 
dt_merge[, AGE := as.numeric(difftime(Index_date, BIRTH_DATE, 
                                      units = "days")) / 365.25]
dt_merge[, AGE_GROUP := cut(AGE, breaks = seq(0, 120, by = 10), right = FALSE, 
                            labels = paste(seq(0, 110, by = 10), 
                                           seq(10, 120, by = 10), sep = "-"))]

#===============================================================================
# step2: merge basic, first date, Outcomes
dt_f <- data.table()
for (d in names(disease_list)) {
  outcome <- disease_list[[d]]
  d_tmp <- merge(dt_merge, outcome , by = "ID", all.x = TRUE,
                 allow.cartesian=TRUE)
  d_tmp[, event := ifelse(is.na(DATE), 0, 1)] 
  d_tmp[, followup := DATE - Index_date]
  setnames(d_tmp, "event", paste0(d,"_event"))
  setnames(d_tmp, "DATE", paste0(d,"_DATE"))
  setnames(d_tmp, "followup", paste0(d,"_followup"))
  if(nrow(dt_f)==0){
    dt_f <- d_tmp
  }else{
    dt_f <- rbind(dt_f, d_tmp, fill = TRUE)
  }
}
head(dt_f)

summary(dt_f)
csv_file_name <- "C:/Users/USER/Downloads/disease_df/dt_f.csv"
fwrite(dt_f, file = csv_file_name, row.names = FALSE)
dim(dt_f)
length(unique(dt_f$ID))
names(dt_f)

#===============================================================================
# todo
# cal end date 
# fill Death date col => fill 疾病_date col
# add follow up date col => leave data with follow up date col > 365
# fill na date
end_date <- max(d_tmp$DATE, na.rm = TRUE)
d_tmp[is.na(DEATH_DATE), DEATH_DATE := end_date]
d_tmp[is.na(DATE), DATE:= pmin(DEATH_DATE, end_date)]

# drop date - index_date < 365
d_tmp[, followup := DATE - Index_date] # add event column
d_tmp <- d_tmp[followup > 365]
setnames(d_tmp, "followup", paste0(d,"_followup"))

# data summary
earliest_values <- tb1_target[order(Index_date)][1:21]
table(tb1_target$Index_date)
