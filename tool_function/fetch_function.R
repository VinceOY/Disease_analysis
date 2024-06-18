library(assertthat)
library(data.table)

fetch_data <- function(dt, target_ID_cols, disease_ID_cols, disease_codes){
  # parameters:
  #   - data(data_table): 包含需要處理的資料
  #   - target_ID_cols(vector): 要標準化的目標 ID 列表(ID, DATE) 
  #   - disease_ID_cols(vector): 疾病 ID 所在的欄位 (疾病col1, 疾病col2,...)
  #   - disease_codes(vector): 要配對的疾病碼 
  # return:
  #   - data_table
    
  # data type restrictions
  assert_that(is.data.table(dt), msg="Error: 'df' must be a data.table")
  assert_that(is.vector(target_ID_cols), 
              msg="Error: 'target_ID_cols' must be a list.")
  assert_that(is.character(disease_codes), 
              msg="Error: 'search_ID' must be a character vector.")
  
  # select columns
  dt <- dt[, c(target_ID_cols,disease_ID_cols),with = FALSE]
  
  # transfer disease_ID_cols type to str
  dt <- dt[, (disease_ID_cols) := lapply(.SD, as.character), 
           .SDcols = disease_ID_cols]
  
  # step1: melt disease_ID 
  melted_data <- melt(dt, id.var=target_ID_cols)
  melted_data <- as.data.table(melted_data)
  
  # step2: match disease codes
  melted_data[, value := as.character(value)]
  filtered_data <- melted_data[grepl(paste0("^", paste(disease_codes, 
                                                       collapse="|^")), value)]
  
  filtered_data <- filtered_data[, c(target_ID_cols[1], target_ID_cols[2]),
                                 with = FALSE]
  
  return(filtered_data)
}

get_valid_data <- function(dt, group_id_col, date_col, k){
  # parameters:
  #   - dt(data_table): 門診資料
  #   - group_id_col: 病人ID欄位
  #   - date_col: 日期欄位
  #   - k(numeric): 一年內看診次數
  # return:
  #   - data_table 一年內超過k筆數據的資料
    
  # data type restrictions
  assert_that(is.data.table(dt), msg="Error: 'dt' must be a data.table")
  assert_that(is.character(group_id_col), 
              msg="Error: 'group_id_col' must be a character")
  assert_that(is.character(date_col), 
              msg="Error: 'date_col' must be a character")
  assert_that(is.numeric(k), msg="Error: 'k' must be a numeric.")
  
  # rename data col
  dt <- dt[, .(ID = get(group_id_col), 
               DATE = get(date_col))]
  # drop duplicate
  dt <- dt[!duplicated(dt)]
  
  # sort values by (ID, DATE)
  dt <- dt[order(ID, DATE)]
  
  # create shift col
  dt[, k_times_data := shift(DATE, n = -(k-1)), by = ID]
  
  # clean data
  dt[, diff := k_times_data - DATE]
  dt <- dt[diff <= 365]
  dt <- dt[, .(ID, DATE)]
  
  return(dt)
}

find_earliest_date <- function(P_list) {
  # parameters:
  #   - P: 參數list包括: example
  #       list(list(df1 = dt_c, idcol = "CHR_NO", datecol = "OPD_DATE", k = 2),
  #       list(dt2....)
  # return:
  #   - data_table 各確診病人的第一次確診時間(ID, DATE)

  combined_data <- data.table()
  for (P in P_list) {
    valid_data <- get_valid_data(P$df, P$idcol, P$datecol, P$k)
    combined_data <- rbind(combined_data, valid_data)
  }
 
  # find earliest date for each ID
  combined_data <- combined_data[, .(Date = min(DATE)), by = ID]
  
  return(combined_data)
}

fetch_data2 <- function(dt, target_ID_cols, disease_ID_cols, disease_codes){
  # parameters:
  #   - data(data_table): 包含需要處理的資料
  #   - target_ID_cols(vector): 要標準化的目標 ID 列表(ID, DATE) 
  #   - disease_ID_cols(vector): 疾病 ID 所在的欄位 (疾病col1, 疾病col2,...)
  #   - disease_codes(list): 要配對的疾病碼 
  # return:
  #   - data_table
  
  # data type restrictions
  assert_that(is.data.table(dt), msg="Error: 'df' must be a data.table")
  assert_that(is.vector(target_ID_cols), 
              msg="Error: 'target_ID_cols' must be a list.")
  assert_that(is.list(disease_codes), 
              msg="Error: 'search_ID' must be a character vector.")
  
  # select columns
  dt <- dt[, c(target_ID_cols,disease_ID_cols),with = FALSE]
  
  # transfer disease_ID_cols type to str
  dt <- dt[, (disease_ID_cols) := lapply(.SD, as.character), 
           .SDcols = disease_ID_cols]
  
  # step1: melt disease_ID 
  melted_data <- melt(dt, id.var=target_ID_cols)
  melted_data <- as.data.table(melted_data)
  melted_data[, value := as.character(value)]
  
  # step2: match disease codes
  dt_list <- list()
  for (i in names(disease_codes)) {
    filtered_data <- melted_data[grepl(paste0("^", paste(disease_codes[[i]], 
                                                         collapse="|^")), value)]
    filtered_data <- filtered_data[, c(target_ID_cols[1], target_ID_cols[2]),
                                   with = FALSE]
    dt_list[[i]] <- filtered_data
  }
  
  return(dt_list)
}