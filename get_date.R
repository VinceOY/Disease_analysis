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
ICD9_codes_Stroke = c("430","431","432","433","434","435","436")
ICD10_codes_Stroke = c("I60","I61","I62","I63","I64","I65","I66","I67","I68",
                       "I69")
disease_code_list[["Stroke"]] <- c(ICD9_codes_Stroke, ICD10_codes_Stroke)

#===============================================================================
## get data
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
# tb_disease = [ID, DATE]
disease_list <- list()
total_diseases <- c(target_disease, related_diseases)
for (d in total_diseases) {
  disease <- d
  disease_tmp_list <- list()
  P_tmp_list <- list()
  for (data_set_name in names(parameters$data_sets)) {
    disease_files <- list.files(target_folder_path, 
                                pattern = paste0(d,"_",data_set_name), 
                                full.names = TRUE, ignore.case = TRUE)
    dt_id_col <- parameters$data_sets[[data_set_name]]$id_col
    dt_date_col <- parameters$data_sets[[data_set_name]]$date_col
    dt_valid_times <- parameters$data_sets[[data_set_name]]$k
    dt_disease_tmp <- data.table()
    for (file in disease_files) {
      d_tmp <- fread(file)
      if(nrow(d_tmp)!=0){
        d_tmp <- standardized_date(d_tmp, dt_date_col)
        if(nrow(dt_target)==0){
          dt_disease_tmp <- d_tmp
        }else{
          dt_disease_tmp <- rbind(dt_disease_tmp, d_tmp)
        }
        disease_tmp_list[[data_set_name]] <- dt_disease_tmp
        P_tmp_list <- append(P_tmp_list, list(list(df = disease_tmp_list[[data_set_name]], 
                                                   idcol = dt_id_col, datecol = dt_date_col, 
                                                   k = dt_valid_times)))
      }
    }
  }
  dt_disease_tmp <- find_earliest_date(P_tmp_list)
  disease_list[[disease]] <- dt_disease_tmp
}

dt_target <- disease_list$Diabete
setnames(dt_target , "Date", "Index_date")

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
# merge basic, first date, Outcomes  
for (d in related_diseases) {
  outcome <- disease_list[[d]]
  dt_merge <- merge(dt_merge, outcome , by = "ID", all.x = TRUE)
  dt_merge[, event := ifelse(is.na(Date), 0, 1)]
  setnames(dt_merge, "event", paste0(d,"_event"))
  setnames(dt_merge, "Date", paste0(d,"_Date"))
}

# fill NA
dt_merge[ ,end_date := DEATH_DATE]
date_cols <- names(dt_merge)[sapply(dt_merge, is.date)]
max_dates <- lapply(dt_merge[, ..date_cols], max, na.rm = TRUE)
last_date <- do.call(max, c(max_dates, na.rm = TRUE))
dt_merge[, end_date := ifelse(is.na(end_date), last_date, end_date)]
dt_merge[, end_date := as.Date(end_date, origin = "1970-01-01")]

for (d in related_diseases) {
  y <- paste0(d, "_Date")
  y2 <- paste0(d,"_event")
  dt_merge[, (y) := ifelse(get(y2)==1, get(y), end_date)]
  dt_merge[, (y) := as.Date(get(y), origin = "1970-01-01")]
  dt_merge[, followup := get(y) - Index_date]
  setnames(dt_merge, "followup", paste0(d,"_followup"))
}

dim(dt_merge)
length(unique(dt_merge$ID))


csv_file_name <- "C:/Users/USER/Downloads/disease_df/dt_f.csv"
fwrite(dt_merge, file = csv_file_name, row.names = FALSE)





