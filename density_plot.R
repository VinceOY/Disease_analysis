rm(list=ls());gc()
library(data.table)
library(stringr)
library(ggplot2)

#===============================================================================
# set parameter
parameters <- list(
  input_path = "C:/Users/USER/Downloads/proj_data/step3/",
  output_path = "C:/Users/USER/Downloads/proj_data/step4/",
  Test_item = list(HbA1c = list(ID = c("014701","F09006B"), 
                                unit = c("%"),
                                upper = 13),
                   ALBUMIN = list(ID = c("010301","11D101","F09038C"), 
                                  unit = c("(?i) g/dl"),
                                  upper = 30),
                   Uric  = list(ID = c("011001","F09013C"), 
                                unit = c("(?i) mg/dl"),
                                upper = 20),
                   HDL = list(ID = c("F09043A", "011301"), 
                              unit = c("(?i) mg/dl"),
                              upper = 120),
                   LDL = list(ID = c("F09044A", "011401"), 
                              unit = c("(?i) mg/dl"),
                              upper = 200),
                   Creatinine = list(ID = c("F09015C","11D101","11A201", "010801","011C01"), 
                                     unit = c("(?i) mg/dl"),
                                     upper = 10)),
  outcome_diseases = c("EyeComp", "CardioDisease", "CerebroDisease", 
                       "PeripheralVascDisease", "Nephropathy", "DiabeticNeuro")
)
input_path <- parameters$input_path
output_path <- parameters$output_path
Test_item <- parameters$Test_item
outcome_diseases <- parameters$outcome_diseases

#===============================================================================
file_name <- paste0(input_path,"lab_result_swt.csv")
dt_lab <- fread(file_name)
dt_dist <- data.table()

# find disease lab + add interval_col
for (t in names(Test_item)) {
  Test_ID <- Test_item[[t]]$ID
  unit_p <- Test_item[[t]]$unit
  upper <- Test_item[[t]]$upper
    
  # select test_item
  dt_test <- dt_lab[grepl(paste0("^", paste(Test_ID, collapse="|^")), 
                          Test_item)]
  # clean test values
  dt_test <- dt_test[, `:=`(clean_value = str_replace_all(VALUE, unit_p, ""), 
                            unit = unit_p)]
  dt_test <- dt_test[, outliers := ifelse(grepl("[><]", clean_value), 1, 0)]
  
  dt_test <- dt_test[, numeric_value := as.numeric(clean_value)]
  
  dt_test <- dt_test[, na_col := ifelse(is.na(numeric_value), 1, 0)]

  # select diabete
  #dt_outcome <- fread(paste0(input_path,"dt_exclude1.csv"))
  #dt_test_T <- merge(dt_outcome, dt_test, by = "ID", all.x = TRUE) # 得糖尿病沒檢驗的人
  #dt_test_T <- dt_test_T[, exclude_testdate_na := ifelse(is.na(Test_date), 1, 0)]
  #dt_test_T <- dt_test_T[, followup := as.numeric(Test_date-Index_date)]

  #check quantile and density plot
  test <- dt_test[!is.na(numeric_value)]
  
  quantiles <- quantile(test$numeric_value, probs = c(0.01, 0.05, 0.25, 0.5, 
                                                      0.75, 0.95, 0.99))
  q1 <- quantiles["25%"]
  median <- quantiles["50%"]
  q3 <- quantiles["75%"]
  iqr <- IQR(test$numeric_value)
  q1_1.5IQR <- q1 - 1.5 * iqr
  q3_1.5IQR <- q3 + 1.5 * iqr
  max_value <- max(test$numeric_value)
  min_value <- min(test$numeric_value)
  d_tmp <- data.table(Test = t, "1%" = quantiles["1%"], "5%" = quantiles["5%"], 
                      "q1" = q1, "median" = median, "q3" = q3, 
                      "95%" = quantiles["95%"], "99%" = quantiles["99%"],
                      "q1_1.5IQR" = q1_1.5IQR, "q3_1.5IQR"= q3_1.5IQR,
                      "max" = max_value, "min" = min_value)
  dt_dist <- rbind(dt_dist,d_tmp)

  test[, numeric_value := ifelse(numeric_value > upper, upper, numeric_value)]
  p <- ggplot(test, aes(x = numeric_value, color = hospital)) +
    geom_density() +
    labs(x = "values", y = "Density", title = paste0(t, " Density Plot"))
  print(p)
  image_name <- paste0(output_path, t,"density_plot_all.png")
  ggsave(image_name, plot = p, width = 6, height = 4, units = "in")
} 

csv_file_name <- paste0(output_path, "test_value_density_all.csv")
fwrite(dt_dist, file = csv_file_name, row.names = FALSE)

