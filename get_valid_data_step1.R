rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/diabetes/"
setwd(new_dir)
source("tool_function/fetch_function.R")
source("tool_function/standard_function.R")
library(stringr)
#===============================================================================
# set parameters
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/TMUCRD_2021_csv/",
  output_path = "C:/Users/USER/Downloads/proj_data/step1/",
  data_sets = list(
    opd = list(
      file_list = c("v_opd_basic_w.csv","v_opd_basic_t.csv",
                    "v_opd_basic_s.csv"),
      disease_ID_cols = c("ICD9_CODE1", "ICD9_CODE2", "ICD9_CODE3", 
                          "ICD10_CODE1", "ICD10_CODE2", "ICD10_CODE3", 
                          "ICD10_CODE4", "ICD10_CODE5"),
      id_col = "CHR_NO",
      date_col = "IPD_DATE"
    ),
    ipd = list(
      file_list = c("v_ipd_basic_w.csv","v_ipd_basic_t.csv",
                    "v_ipd_basic_s.csv"),
      disease_ID_cols = c("ICD10_CODE1", "ICD10_CODE2", "ICD10_CODE3", 
                          "ICD10_CODE4", "ICD10_CODE5", "ICD10_CODE6", 
                          "ICD10_CODE7"),
      id_col = "CHR_NO",
      date_col = "IPD_DATE"
    )
  )
)
input_path <- parameters$input_path
output_path <- parameters$output_path
#===============================================================================
## get disease code list
dt1 <- fread(paste0(input_path, "ICD92ICD10.csv"))
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
                  Hypertension = "401", 
                  PeripheralEnthe = "726",
                  UnknownCauses = "799", 
                  LipoidMetabDis = "272", 
                  AcuteURI = "465", 
                  AbdPelvicSymptoms = c("724","789"),
                  Dermatophytosis = "110", 
                  GenSymptoms = "780",
                  RespChestSymptoms = "786", 
                  HeadNeckSymptoms = "784",
                  ContactDermEczema = "692", 
                  ViralInfection = "79",
                  ObesityHyperal = "278", 
                  JointDisorders = "719",
                  AcuteBronchitis = "466", 
                  SoftTissueDis = "729",
                  BloodExamFindings = "790", 
                  RefractionDis = "367",
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

dt_code <- data.table(disease_name = character(),
                      ICD_code = character(),
                      stringsAsFactors = FALSE)

for (disease in names(disease_code_list)) {
  codes <- disease_code_list[[disease]]
  for (code in codes) {
    dt_code <- rbind(dt_code, data.table(disease_name = disease, 
                                         ICD_code = code, 
                                         stringsAsFactors = FALSE))
  }
}
write.csv(dt_code, paste0(output_path, "disease_code_list.csv"), 
          row.names = FALSE)
#===============================================================================
# get data
disease_codes <- fread(paste0(output_path,"disease_code_list.csv")) # convert list
disease_codes <- disease_codes[, list(ICD_code = list(ICD_code)), 
                               by = disease_name]
disease_codes <- setNames(disease_codes$ICD_code, disease_codes$disease_name)
for (data_set_name in names(parameters$data_sets)) {
  data_set <- parameters$data_sets[[data_set_name]]
  dt_file_list <- data_set$file_list
  dt_disease_ID_cols <- data_set$disease_ID_cols
  dt_id_col <- data_set$id_col
  dt_date_col <- data_set$date_col
  dt_target_ID_cols <- c(dt_id_col, dt_date_col)
  
  for (file in dt_file_list) {
    hospital_names <- gsub(".*_(.*)\\.csv", "\\1", file)
    d_tmp <- fread(paste0(input_path, file))
    merge_df <- fetch_data2(d_tmp, dt_target_ID_cols, dt_disease_ID_cols, 
                            disease_codes)
    for (name in names(merge_df)) {
      csv_file_name <- paste0(output_path, name, "_",
                              data_set_name, "_", hospital_names, ".csv")
      fwrite(merge_df[[name]], file = csv_file_name, row.names = FALSE)
      print(csv_file_name)
    }
  }
}


