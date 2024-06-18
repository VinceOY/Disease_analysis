rm(list=ls());gc()
source("tool_function/standard_function.R")
library(data.table)
library(stringr)
#===============================================================================
# define parameters: 
# related_diseases: 目標疾病, outcome, control 
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/step4/",
  output_path = "C:/Users/USER/Downloads/proj_data/step5/",
  related_diseases = c("Diabete","EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro",
                       "Hypertension","PeripheralEnthe","UnknownCauses",
                       "LipoidMetabDis","AcuteURI","AbdPelvicSymptoms",
                       "Dermatophytosis","GenSymptoms","RespChestSymptoms",
                       "HeadNeckSymptoms","ContactDermEczema","ViralInfection",
                       "ObesityHyperal", "JointDisorders", "AcuteBronchitis", 
                       "SoftTissueDis", "BloodExamFindings", "RefractionDis",
                       "ConjunctivaDis")
)
input_path <- parameters$input_path
output_path <- parameters$output_path
related_diseases <- parameters$related_diseases
#===============================================================================
# 檢驗結果: LAB result 
# 兩院結果合併、select column, rename
result_files <- c("v_labresult_t.csv", "v_labresult_s.csv")
d_result <- data.table()
for (file in result_files) {
  hospital_names <- gsub(".*_(.*)\\.csv", "\\1", file)
  d_tmp <- fread(paste0(input_path, file))
  d_tmp[,hospital:= hospital_names]
  d_result <- rbind(d_result, d_tmp)
}
d_result <- d_result[,c("CHR_NO", "hospital","P_DATE","R_ITEM","VALUE"), 
                     with = FALSE]
d_result <- standardized_date(d_result, "P_DATE")
setnames(d_result, "CHR_NO", "ID")
setnames(d_result, "P_DATE", "Test_date")
setnames(d_result, "R_ITEM", "Test_item")

#===============================================================================
# 檢驗結果2: v_exper_sign_w.csv 萬芳
# 萬芳結果select column, rename
d_result_w <- fread(paste0(input_path, "v_exper_sign_w.csv"))
d_result_w[,hospital:= "w"]
d_result_w <- d_result_w[,c("CHR_NO", "hospital","EXPER_DATE","GROUP_CODE",
                            "EXPER_DATA2"), with = FALSE]
d_result_w <- standardized_date(d_result_w, "EXPER_DATE")
setnames(d_result_w,"CHR_NO","ID")
setnames(d_result_w, "EXPER_DATE", "Test_date")
setnames(d_result_w, "GROUP_CODE", "Test_item")
setnames(d_result_w, "EXPER_DATA2", "VALUE")

#===============================================================================
# merge: lab results(s, w, t) and output csv
lab_result <- rbind(d_result, d_result_w)
csv_file_name <- paste0(output_path,"lab_result_swt.csv")
fwrite(lab_result, file = csv_file_name, row.names = FALSE)
