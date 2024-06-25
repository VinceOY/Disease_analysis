rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
library(data.table)
library(stringr)
library(survival)

#===============================================================================
# set parameter
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/step4/",
  output_path = "C:/Users/USER/Downloads/proj_data/step5/",
  Test_item = c("HbA1c", "ALBUMIN"),
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
)
input_path <- parameters$input_path
output_path <- parameters$output_path
Test_item <- parameters$Test_item
outcome_diseases <- parameters$outcome_diseases

#===============================================================================
# 檢驗代號: EXP ITEM 
dt_lab <- fread("C:/Users/USER/Downloads/proj_data/step3/lab_result_swt.csv")

item_files <- c("v_exp_item_t.csv", "v_exp_item_s.csv")
d_item <- data.table()
for (file in item_files) {
  d_tmp <- fread(paste0(input_path, file))
  d_item <- rbind(d_item, d_tmp)
}
d_item$
d_item <- d_item[,c("O_ITEM", "R_ITEM","R_ITEM_NAME"), with = FALSE]

dt_selected <- d_item[grep("Glucose", R_ITEM_NAME, ignore.case = TRUE)]
selected_ID <- unique(dt_selected[["R_ITEM"]])


for (i in selected_ID) {
  dt_tmp <- dt_lab[Test_item == i]
  cat(i, nrow(dt_tmp),"\n")
}

