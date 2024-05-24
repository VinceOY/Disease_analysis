rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/fetch_function.R")
source("tool_function/standard_function.R")
library(stringr)
#===============================================================================
## get disease code list
folder_path2 = "C:/Users/USER/Downloads/proj/"
dt1 <- fread(paste0(folder_path2, "ICD92ICD10.csv"))
dt1 <- dt1[, c("ICD-9-CM代碼" ,"ICD-10-CM"),with = FALSE]
setnames(dt1, "ICD-9-CM代碼", "ICD9")
setnames(dt1, "ICD-10-CM", "ICD10")
ICD9_diabetes = list(Eye_complications = c("362.01","362.02","362.55", "362.11",
                                           "365.44","369","361","369.60"),
                     Cardiovascular_disease = c("414.9","413.9","411.1","412",
                                                "428","425","402"),
                     Cerebrovascular_disease = c("435.9","434.91","432.9",
                                                 "437.1"),
                     Peripheral_vascular_disease = c("440.2","440.20","440.21",
                                                     "440.22","440.23","440.24"),
                     Nephropathy = c("583", "585", "V451"),
                     Diabetic_neuropathy = c("357.2","337.1","353"),
)

disease_code_list = list()
for (a in names(ICD9_diabetes)) {
  d_tmp <-  dt1[grepl(paste0("^", paste(ICD9_diabetes[[a]], 
                                        collapse="|^")), ICD9)]
  disease_code_list[[a]] <- c(d_tmp$ICD9,d_tmp$ICD10)
}

# control disease code transfer
control_disease <- c("401","726","799","272","465","724","789","110","780","786",
                     "784","692","79","278","719","466","729","790","367","372")

control_df <- dt1[grepl(paste0("^", paste(control_disease, 
                                             collapse="|^")), ICD9)]
#=== output

disease_code_list[["Diabete"]] <- c("E08","E09","E10","E11","E12")
disease_code_list[["Contorl_disease"]] <- c(control_df$ICD9,control_df$ICD10)

#===============================================================================
## get data
parameters <- list(
  # files
  folder_path = "C:/Users/USER/Downloads/hospital/TMUCRD_2021_csv_new/",
  disease_codes = disease_code_list,
  # data sets parameters
  data_sets = list(
    opd = list(
      # dt1 parameters
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
      # dt2 parameters
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

P_list = list()
folder_path <- parameters$folder_path
disease_codes <- parameters$disease_codes

for (data_set_name in names(parameters$data_sets)) {
  data_set <- parameters$data_sets[[data_set_name]]
  dt_file_list <- data_set$file_list
  dt_id_col <- data_set$id_col
  dt_date_col <- data_set$date_col
  dt_target_ID_cols <- c(dt_id_col, dt_date_col)
  dt_disease_ID_cols <- data_set$disease_ID_cols
  
  for (file in dt_file_list) {
    d_tmp <- fread(paste0(folder_path, file))
    merge_df <- fetch_data2(d_tmp, dt_target_ID_cols, dt_disease_ID_cols, 
                            disease_codes)
    for (name in names(merge_df)) {
      csv_file_name <- paste0("C:/Users/USER/Downloads/proj/disease_df/",
                              data_set_name,"_", name, ".csv")
      fwrite(merge_df, file = csv_file_name, row.names = FALSE)
    }
  }
}

