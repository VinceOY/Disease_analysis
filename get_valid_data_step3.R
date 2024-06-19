rm(list=ls());gc()
#===============================================================================
# set parameter
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/step2/",
  output_path = "C:/Users/USER/Downloads/proj_data/step3/",
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro"),
  select_col = c("ID", "SEX_TYPE", "BIRTH_DATE", "DEATH_DATE", "Index_date",
                  "AGE", "AGE_GROUP", "Hypertension_event", 
                  "PeripheralEnthe_event", "UnknownCauses_event", 
                  "LipoidMetabDis_event", "AcuteURI_event" ,     
                  "AbdPelvicSymptoms_event", "Dermatophytosis_event",   
                  "GenSymptoms_event", "RespChestSymptoms_event", 
                  "HeadNeckSymptoms_event", "ContactDermEczema_event",
                  "ViralInfection_event", "ObesityHyperal_event",
                  "JointDisorders_event", "AcuteBronchitis_event",
                  "SoftTissueDis_event", "BloodExamFindings_event",
                  "RefractionDis_event", "ConjunctivaDis_event")
)
input_path <- parameters$input_path
output_path <- parameters$output_path
outcome_diseases <- parameters$outcome_diseases
select_col <- parameters$select_col
#===============================================================================
## exclude: 1. ID, 2. AGE, 3. Index Date, 4. outcome date followup 
# output clean
dt_merge <- fread(paste0(input_path, "dt_merge.csv"))
print(paste0("# of exclude id: ", 
             length(unique(dt_merge[dt_merge[,exclude_ID==1]]$ID))))

# AGE
dt_merge[, exclude_AGE := ifelse(AGE < 20 | AGE > 100, 1, 0)]
print(paste0("# of exclude age: ", 
             length(unique(dt_merge[dt_merge[, exclude_AGE==1&
                                               exclude_ID==0]]$ID))))
# valid index date # by year 判斷 double check
valid_Index_date <- min(dt_merge$Index_date)+365 
dt_merge[, exclude_Indexdate := ifelse(Index_date < valid_Index_date, 1, 0)]
print(paste0("# of not enough observe date:  ", 
             length(unique(dt_merge[dt_merge[, exclude_Indexdate==1&
                                               exclude_ID==0&
                                               exclude_AGE==0]]$ID))))
# check # of patients 
print(paste0("# of patients: ", 
             length(unique(dt_merge[dt_merge[, exclude_Indexdate==0&
                                               exclude_ID==0&
                                               exclude_AGE==0]]$ID))))
# output
dt_exlcude <- dt_merge[dt_merge[, exclude_Indexdate==0&
                                  exclude_ID==0&
                                  exclude_AGE==0]]

csv_file_name <- paste0(output_path,"dt_exclude1.csv")
fwrite(dt_exlcude, file = csv_file_name, row.names = FALSE)                                                                                             

#===============================================================================
# outcome date followup
dt_exlcude <- fread(paste0(output_path, "dt_exclude1.csv"))

outcome_list <- list()
for (o in outcome_diseases) {
  col <- paste0(o,"_followup")
  dt_exlcude[, exclude_outcome1 := ifelse(get(col) < 0, 1, 0)]
  dt_exlcude[, exclude_outcome2 := ifelse(get(col) <= 365 & get(col) >= 0, 1, 0)]
  d_tmp <- dt_exlcude
  need_col <- c(select_col, paste0(o,"_Date"), paste0(o,"_event"),
                paste0(o,"_followup"), "exclude_outcome1", "exclude_outcome2")
  d_tmp <- d_tmp[, ..need_col]
  
  print(paste0("# of before index_date: ", 
               length(unique(d_tmp[d_tmp[,(exclude_outcome1==1)]]$ID))))
  
  print(paste0("# of after index_date less than 1 year: ", 
               length(unique(d_tmp[d_tmp[,(exclude_outcome2==1)]]$ID))))
  
  dt_clean <- d_tmp[d_tmp[,(exclude_outcome1==0&exclude_outcome2==0)]]
  
  print(paste0("# of patients: ", length(unique(dt_clean$ID))))
  
  csv_file_name <- paste0(output_path, o, "_exclude2.csv")
  fwrite(dt_clean, file = csv_file_name, row.names = FALSE)
}

