rm(list=ls());gc()
new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
library(data.table)
library(stringr)

#===============================================================================
# define parameters: 
# Test_item: test_item name, unit
# exclude_columns: 清除age, ID, index_date
# target_folder_path: 檔案存放路徑
# outcome_diseases: DM related diseases
parameters <- list(
  target_folder_path = "C:/Users/USER/Downloads/disease_df/",
  Test_item = list(HbA1c = list(ID = c("014701","F09006B"), unit = c("%")),
                   ALBUMIN = list(ID = c("010301","11D101","F09038C"), 
                                  unit = c("(?i) g/dl"))),
  exclude_columns = c("exclude_AGE", "exclude_ID", "exclude_Indexdate"),
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
)
exclude_columns <- parameters$exclude_columns
target_folder_path <- parameters$target_folder_path
outcome <- parameters$outcome_diseases
Test_item <- parameters$Test_item
outcome_file <- paste0(outcome[1],"_clean.csv") # for table1 & table2 



#=================================================================
# cal mean by ppl 
# input: clean_people_lab.csv 最終人選的檢驗資料(含基本資料) * 12 files (test+outcome)
for(f in files){
  dt <- read(f)  
  for(n in ID){
    cal(Q0~Q4(test_item_mean))
  }
  csv_file_name
  fwrite()
}
# output: input_x包含Q1~Q4(test_item: mean、sd、cv、rms)歸人(N) * 12 files (test_outcome_dtf)

#=================================================================
# build model table: 
model_result <- data.table(test_item = c(), outcome = c(), var = c(), HR = c(), CI = c())
for(t in test){
  for(o in outcomes){
    dt <- fread(output)
    for(i in inputs){
      select_col <- i
      d_tmp <- dt[,.select_col]
      x <- input_x
      y <- input_y
      build model	
      model_tmp <- model summary[,c(var、HR、CI)]
      model_tmp[, test_item := t]
      model_tmp[, outcome := output_y]
      model_result <- rbind(model_result, model_tmp)
    }
  }
}
