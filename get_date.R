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
  d_tmp <-  dt1[grepl(paste0("^", paste(ICD9_codes[[a]],collapse="|^")), ICD9)]
  d_tmp[,disease_name := a]
  disease_code_list[[a]] <- c(unique(d_tmp$ICD9),unique(d_tmp$ICD10))
}
ICD10_codes_diabete = c("E08","E09","E10","E11","E12")
d_tmp <- dt1[grepl(paste0("^", paste(ICD10_codes_diabete, 
                                     collapse="|^")), ICD10)]                              
d_tmp[,disease_name := "diabete"]
disease_code_list[["diabete"]] <- c(unique(d_tmp$ICD9), unique(d_tmp$ICD10))
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
      date_col = "OPD_DATE"
    ),
    ipd = list(
      # dt2 parameters
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
    hospital_names <- sub(".*_(.*)\\.csv", "\\1", file)
    d_tmp <- fread(paste0(folder_path, file))
    merge_df <- fetch_data2(d_tmp, dt_target_ID_cols, dt_disease_ID_cols, 
                            disease_codes)
    for (name in names(merge_df)) {
      csv_file_name <- paste0("C:/Users/USER/Downloads/disease_df/", name, "_",
                              data_set_name, "_", hospital_names, ".csv")
      fwrite(merge_df[[name]], file = csv_file_name, row.names = FALSE)
      print(csv_file_name)
    }
  }
}

#===============================================================================
# tb1_diabete: ID, Index_date 
# data source: opd/ipd_3院_diabete檔案6個
# step1: rbind 成 opd / ipd 2 files 
# step2: 標準化日期
# step3: append p_list
# step4: find_earliest_date(P_list)
# output: tb_diabete[ID, Index_date]
parameters <- list(
  disease_folder_path = "C:/Users/USER/Downloads/disease_df/",
  data_sets = list(
    Diabete_opd = list(
      id_col = "CHR_NO",
      date_col = "OPD_DATE",
      k = 3
    ),
    Diabete_ipd = list(
      id_col = "CHR_NO",
      date_col = "IPD_DATE",
      k = 1
    )
  )
)  

disease_folder_path <- parameters$disease_folder_path
target_list <- list()
P_list <- list()
for (data_set_name in names(parameters$data_sets)) {
  dt_id_col <- parameters$data_sets[[data_set_name]]$id_col
  dt_date_col <- parameters$data_sets[[data_set_name]]$date_col
  dt_valid_times <- parameters$data_sets[[data_set_name]]$k
  target_files = list.files(disease_folder_path, pattern = data_set_name, 
                            full.names = TRUE, ignore.case = TRUE)
  dt_target <- data.table()
  for (file in target_files) {
    d_tmp <- fread(file)
    d_tmp[, (dt_date_col) := str_pad(get(dt_date_col), width = 7, side = "left",
                                                 pad = "0")]
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
tb1_target <- find_earliest_date(P_list)
setnames(tb1_target, "Date", "Index_date")
top5_values <- tb1_target[order(-Index_date)][1:20]

#tb1_target[,ID_trans := 1:nrow(tb1_target)]
#dt_id <- tb1_target[, c("ID_trans", "ID")]
#===============================================================================
# data source:  病人資料_CHR_basic
# step1: 三院資料合併
# step2: select columns("CHR_NO","SEX_TYPE","BIRTH_DATE","DEATH_DATE")
# step3: drop duplicate by "CHR_NO", rename("CHR_NO", "ID")
# step4: 對"BIRTH_DATE","DEATH_DATE" 轉str, 補0, 標準化
# output: dt_basic[ID, SEX_TYPE,"BIRTH_DATE", "DEATH_DATE"]
folder_path <- "C:/Users/USER/Downloads/TMUCRD_2021_csv/"
basic_files <- c("v_chr_basic_w.csv","v_chr_basic_t.csv","v_chr_basic_s.csv")
d_basic <- data.table()
for (file in basic_files) {
  d_tmp <- fread(paste0(folder_path, file))
  d_basic <- rbind(d_basic, d_tmp)
}
d_basic <- d_basic[, c("CHR_NO","SEX_TYPE","BIRTH_DATE","DEATH_DATE"),
                   with = FALSE]
d_basic[, unknown_ID := ifelse(.N > 1, 1, 0), by = CHR_NO]
cols_to_pad <- c("BIRTH_DATE", "DEATH_DATE")
d_basic[, (cols_to_pad) := lapply(.SD, function(x) 
  str_pad(x, width = 7, side = "left", pad = "0")), .SDcols = cols_to_pad]
d_basic <- standardized_date(d_basic, "BIRTH_DATE") 
d_basic <- standardized_date(d_basic, "DEATH_DATE") 
setnames(d_basic, "CHR_NO", "ID")
#d_basic_n <- merge(dt_id,d_basic, by = "ID", all.y = TRUE)

#===============================================================================
# Merge tables
# step1: merge basic, index_date by CHR_NO  
# step1.1: cal age / sep age group 
dt_merge <- merge(d_basic, tb1_target, by = "ID", all = FALSE) 
print(paste("# of unkown ID:", sum(dt_merge$unknown_ID)))

# cal age / sep age group 
dt_merge[, AGE := as.numeric(difftime(Index_date, BIRTH_DATE, 
                                      units = "days")) / 365.25]
dt_merge[, AGE_GROUP := cut(AGE, breaks = seq(0, 120, by = 10), right = FALSE, 
                            labels = paste(seq(0, 110, by = 10), 
                                           seq(10, 120, by = 10), sep = "-"))]

#===============================================================================
# complication_list: [tb1,tb2,tb3,...]
# tb_complication = [ID, ("疾病","DATE")]
# data source: opd/ipd_3院_complication檔案: 2*3*6 = 36個
# step1: rename("OPD_DATE", ("疾病","DATE") )
# step2: rename("IPD_DATE", ("疾病","DATE") )
# step3: 標準化日期
# step4: rbind成疾病檔
# step5: rename("CHR_NO","ID")
# output: 6個疾病table list
complication_groups <- c("EyeComp", "CardioDisease", "CerebroDisease", 
                         "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
complication_list <- list()
for (c in complication_groups) {
  complication <- c
  complication_files <- list.files(disease_folder_path, pattern = c, 
                                   full.names = TRUE, ignore.case = TRUE)
  dt_c <- data.table()
  for (file in complication_files) {
    d_tmp <- fread(file)
    d_tmp <- standardized_date(d_tmp, names(d_tmp)[2])
    setnames(d_tmp, names(d_tmp)[2], paste0(complication,"_DATE") )
    dt_c <- rbind(dt_c, d_tmp)
  }
  setnames(dt_c,"CHR_NO", "ID")
  complication_list[[complication]] <- dt_c
}

#===============================================================================
# step2: merge basic, first date, Outcomes
# step2.1: create event col 
# step2.2: cal end date 
# step2.3: fill Death date col => fill 疾病_date col
# step2.4: add follow up date col => leave data with follow up date col > 365

dt_merge2 <- dt_merge
for (d in names(complication_list)) {
  d <- names(complication_list)[2]
  outcome <- complication_list[[d]]
  dt_merge2 <- merge(dt_merge2, outcome , by = "ID", all.y = TRUE)
}

outcome
names(dt_merge2)  

  
dx <- data.table(a = c(1,1,1,1,2,2), b = 3:8)
dy <- data.table(a = c(1,1,2), c = 7:9)
merge(dx, dy, by = "a", all = TRUE, allow.cartesian=TRUE)





  
  #  dt_merge2[, event := ifelse(is.na(Date), 0, 1)] # add event column
#  setnames(dt_merge2, event, paset0(d,"_event"))
  # Todo
#  end_date <- max(dt_merge2$Date, na.rm = TRUE)
  # fill Death date col
#  dt_merge2[is.na(DEATH_DATE), DEATH_DATE := end_date]

dt_merge2 <- merge(dt_merge2, outcome, by = "ID", all.x = TRUE)
names(outcome)
unique(outcome$ID)
sum(unique(dt_merge2$ID) %in% outcome$ID)



dt_merge2 <- merge(dt_merge, tb2, by = "ID", all = FALSE) # all = FALSE只保留兩邊都有的
dt_merge2 <- dt_merge2[,c("ID","SEX_TYPE","AGE","age_group","BIRTH_DATE",
                          "DEATH_DATE", "Index_date", "Date")]


dt_merge2[, event := ifelse(is.na(Date), 0, 1)] # add event column

end_date <- max(dt_merge2$Date, na.rm = TRUE)

# fill Death date col
dt_merge2[is.na(DEATH_DATE), DEATH_DATE := end_date]

# fill date col
dt_merge2[is.na(Date), Date:= pmin(DEATH_DATE, end_date)]

na_rows <- dt_merge2[is.na(Date)]

# drop date - index_date < 365
dt_merge2[, observe := Date - Index_date] # add event column
dt_f <- dt_merge2[observe > 365]

# data summary
summary(dt_f)

#Todo:
#dt_merge2 修改



