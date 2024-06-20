rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/diabetes/"
setwd(new_dir)
source("tool_function/fetch_function.R")
source("tool_function/standard_function.R")
library(stringr)
#===============================================================================
# set parameters
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/step1/",
  output_path = "C:/Users/USER/Downloads/proj_data/step2/",
  basic_files = c("v_chr_basic_w.csv","v_chr_basic_t.csv","v_chr_basic_s.csv"),
  related_diseases = c("Diabete","EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro",
                       "Hypertension","PeripheralEnthe","UnknownCauses",
                       "LipoidMetabDis","AcuteURI","AbdPelvicSymptoms",
                       "Dermatophytosis","GenSymptoms","RespChestSymptoms",
                       "HeadNeckSymptoms","ContactDermEczema","ViralInfection",
                       "ObesityHyperal", "JointDisorders", "AcuteBronchitis", 
                       "SoftTissueDis", "BloodExamFindings", "RefractionDis",
                       "ConjunctivaDis"),
  data_sets = list(
    opd = list(
      id_col = "CHR_NO",
      date_col = "OPD_DATE",
      k = 3 # follow disease
    ),
    ipd = list(
      id_col = "CHR_NO",
      date_col = "IPD_DATE",
      k = 1 # follow disease
    )
  )
)
input_path <- parameters$input_path
output_path <- parameters$output_path
basic_files <- parameters$basic_files
related_diseases <- parameters$related_diseases
target_disease <- related_diseases[1]
outcome_diseases <- related_diseases[2:7]
control_diseases <- related_diseases[8:length(related_diseases)]
taget_outcome_diseases <- c(target_disease, outcome_diseases)


#===============================================================================
# dt_basic: ["ID", "SEX_TYPE", "BIRTH_DATE", "DEATH_DATE" ]
dt_basic <- data.table()
for (file in basic_files) {
  d_tmp <- fread(paste0(input_path, file))
  dt_basic <- rbind(dt_basic, d_tmp)
}
dt_basic <- dt_basic[, c("CHR_NO","SEX_TYPE","BIRTH_DATE","DEATH_DATE"),
                     with = FALSE]
cols_to_pad <- c("BIRTH_DATE", "DEATH_DATE")
dt_basic <- standardized_date(dt_basic, "BIRTH_DATE")
dt_basic <- standardized_date(dt_basic, "DEATH_DATE") 
setnames(dt_basic, "CHR_NO", "ID")
dt_basic <- dt_basic[, exclude_ID := ifelse(.N > 1, 1, 0), by = ID]
#===============================================================================
# merge table
# disease_list: [tb1,tb2,tb3,...]
# tb_disease = [ID, DATE]
disease_list <- list()
for (d in taget_outcome_diseases) {
  disease <- d
  disease_tmp_list <- list()
  P_tmp_list <- list()
  for (data_set_name in names(parameters$data_sets)) {
    disease_files <- list.files(input_path,
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
        if(nrow(dt_disease_tmp)==0){
          dt_disease_tmp <- d_tmp
        }else{
          dt_disease_tmp <- rbind(dt_disease_tmp, d_tmp)
        }
        disease_tmp_list[[data_set_name]] <- dt_disease_tmp
        P_tmp_list <- append(P_tmp_list, 
                             list(list(df = disease_tmp_list[[data_set_name]],
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
csv_file_name <- paste0(output_path, "dt_target.csv")
fwrite(dt_target, file = csv_file_name, row.names = FALSE)
length(unique(dt_target$ID))
#===============================================================================
# Merge tables
# step1: merge basic, index_date by CHR_NO and cal age / sep age group 
dt_target <- fread(paste0(output_path, "dt_target.csv"))
dt_merge <- merge(dt_basic, dt_target, by = "ID", all = FALSE) 
dt_merge[, AGE := as.numeric(difftime(Index_date, BIRTH_DATE, 
                                      units = "days")) / 365.25]
dt_merge[, AGE_GROUP := cut(AGE, breaks = seq(0, 120, by = 10), right = FALSE, 
                            labels = paste(seq(0, 110, by = 10), 
                                           seq(10, 120, by = 10), sep = "-"))]

#===============================================================================
# merge control disease # k follow disease
p2 <- list(a = list(type ="_ipd",k=1), b = list(type ="_opd",k=3))
dt_index <- dt_merge[,.(ID,Index_date)]
c <- control_diseases[1]
dp <- p2$a
for (c in control_diseases) {
  ID_list = c()
  control <- c
  for (dp in p2) {
    t <- dp$type
    k <- dp$k
    p <- paste0(control,t)
    control_files <- list.files(input_path, pattern = p, 
                                full.names = TRUE, ignore.case = TRUE)
    dt_c <- data.table()
    for (file in control_files) {
      d_tmp <- fread(file)
      if(nrow(d_tmp)!=0){
        d_tmp <- standardized_date(d_tmp, names(d_tmp)[2])
        dt_c <- rbind(dt_c, d_tmp)
      }
    }
    setnames(dt_c,"CHR_NO", "ID")
    dt_c <- merge(dt_index, dt_c, by = "ID", all.x = TRUE)
    dt_c[, followup := get(names(dt_c)[3]) - Index_date]
    dt_c <- dt_c[followup <= 0 & followup >= -365] 
    filtered_id <- dt_c[ , .N, by = ID][N >= k]
    valid_ID <- unique(dt_c[ID %in% filtered_id$ID]$ID)
    ID_list <- c(ID_list, valid_ID)
  }
  ID_list <- unique(ID_list)
  dt_merge[, (paste0(control,"_event")):= ifelse(ID %in% ID_list, 1, 0)]
}

#===============================================================================
# merge basic, index_date, control disease, Outcomes  
for (d in outcome_diseases) {
  outcome <- disease_list[[d]]
  dt_merge <- merge(dt_merge, outcome , by = "ID", all.x = TRUE)
  dt_merge[, event := ifelse(is.na(Date), 0, 1)]
  setnames(dt_merge, "event", paste0(d,"_event"))
  setnames(dt_merge, "Date", paste0(d,"_Date"))
  
  # fill NA 
  dt_merge[ ,end_date := DEATH_DATE]
  date_cols <- names(dt_merge)[sapply(dt_merge, is.date)]
  max_dates <- lapply(dt_merge[, ..date_cols], max, na.rm = TRUE)
  last_date <- do.call(max, c(max_dates, na.rm = TRUE))
  dt_merge[, end_date := ifelse(is.na(end_date), last_date, end_date)]
  dt_merge[, end_date := as.Date(end_date, origin = "1970-01-01")]
  
  y <- paste0(d, "_Date")
  y2 <- paste0(d,"_event")
  dt_merge[, (y) := as.Date(ifelse(get(y2) == 1, get(y), end_date), 
                            origin = "1970-01-01")]
  dt_merge[, (paste0(d, "_followup")) := get(y) - as.Date(Index_date)]
}

print(paste0("# of patients: ", length(unique(dt_merge$ID))))
csv_file_name <- paste0(output_path, "dt_merge.csv")
fwrite(dt_merge, file = csv_file_name, row.names = FALSE)
dim(dt_merge)
