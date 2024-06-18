
#===============================================================================
# Merge tables
# step1: merge basic, index_date by CHR_NO and cal age / sep age group 
dt_merge <- merge(dt_basic, dt_target, by = "ID", all = FALSE) 
dt_merge[, AGE := as.numeric(difftime(Index_date, BIRTH_DATE, 
                                      units = "days")) / 365.25]
dt_merge[, AGE_GROUP := cut(AGE, breaks = seq(0, 120, by = 10), right = FALSE, 
                            labels = paste(seq(0, 110, by = 10), 
                                           seq(10, 120, by = 10), sep = "-"))]

#===============================================================================
# merge control disease
p2 <- list(a = list(type ="_ipd",k=1), b = list(type ="_opd",k=3))
dt_index <- dt_merge[,.(ID,Index_date)]
for (c in control_diseases) {
  ID_list = c()
  control <- c
  for (dp in p2) {
    t <- dp$type
    k <- dp$k
    p <- paste0(control,t)
    control_files <- list.files(target_folder_path, pattern = p, 
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
# 合併整段
outcome_diseases
for (d in outcome_diseases) {
  outcome <- disease_list[[d]]
  dt_merge <- merge(dt_merge, outcome , by = "ID", all.x = TRUE)
  dt_merge[, event := ifelse(is.na(Date), 0, 1)]
  setnames(dt_merge, "event", paste0(d,"_event"))
  setnames(dt_merge, "Date", paste0(d,"_Date"))
}
head(dt_merge)

# fill NA 可以往上合併
dt_merge[ ,end_date := DEATH_DATE]
date_cols <- names(dt_merge)[sapply(dt_merge, is.date)]
max_dates <- lapply(dt_merge[, ..date_cols], max, na.rm = TRUE)
last_date <- do.call(max, c(max_dates, na.rm = TRUE))
dt_merge[, end_date := ifelse(is.na(end_date), last_date, end_date)]
dt_merge[, end_date := as.Date(end_date, origin = "1970-01-01")]

for (d in outcome_diseases) {
  y <- paste0(d, "_Date")
  y2 <- paste0(d,"_event")
  dt_merge[, (y) := ifelse(get(y2)==1, get(y), end_date)]
  dt_merge[, (y) := as.Date(get(y), origin = "1970-01-01")]
  dt_merge[, followup := get(y) - Index_date]
  setnames(dt_merge, "followup", paste0(d,"_followup"))
}
print(paste0("# of patients: ", length(unique(dt_merge$ID))))



#===============================================================================
## exclude: 1. ID, 2. AGE, 3. Index Date, 4. outcome date followup 
# basic 順序、exclude 修改
# ID
dt_merge[, exclude_ID := ifelse(.N > 1, 1, 0), by = ID]
print(paste0("# of exclude id: ", 
             length(unique(dt_merge[dt_merge[,exclude_ID==1]]$ID))))

# AGE
dt_merge[, exclude_AGE := ifelse(AGE < 20 | AGE > 100, 1, 0)]
print(paste0("# of exclude age: ", 
             length(unique(dt_merge[dt_merge[, exclude_AGE==1&
                                               exclude_ID==0]]$ID))))

# valid index date # by year 判斷
valid_Index_date <- min(dt_merge$Index_date)+365 
dt_merge[, exclude_Indexdate := ifelse(Index_date < valid_Index_date, 1, 0)]
print(paste0("# of not enough observe date:  ", 
             length(unique(dt_merge[dt_merge[, exclude_Indexdate==1&
                                               exclude_ID==0&
                                               exclude_AGE==0]]$ID))))
# step1: check # of patients 
# clean_df
print(paste0("# of patients: ", 
             length(unique(dt_merge[dt_merge[, exclude_Indexdate==0&
                                               exclude_ID==0&
                                               exclude_AGE==0]]$ID))))
# outcome date followup
outcome_list <- list()

for (o in outcome_diseases) {
  col <- paste0(o,"_followup")
  dt_merge[, exclude_outcome1 := ifelse(get(col) < 0, 1, 0)]
  dt_merge[, exclude_outcome2 := ifelse(get(col) <= 365 & get(col) >= 0, 1, 0)]
  d_tmp <- dt_merge
  # setname
  outcome_col <- c(names(d_tmp)[1:26], paste0(o,"_Date"),paste0(o,"_event"), 
                   paste0(o,"_followup"), names(d_tmp)[46:50])
  d_tmp <- d_tmp[, ..outcome_col]
  outcome_list[[o]] <- d_tmp 
  # ! by f"d;6count => by sume $e;::
  print(paste0("# of out of range1 ", o, ": ", 
               length(unique(d_tmp[d_tmp[,(exclude_ID==0&
                                             exclude_AGE==0&
                                             exclude_Indexdate==0&
                                             exclude_outcome1==1&
                                             exclude_outcome2==0)]]$ID))))
  print(paste0("# of out of range2 ", o, ": ", 
               length(unique(d_tmp[d_tmp[,(exclude_ID==0&
                                             exclude_AGE==0&
                                             exclude_Indexdate==0&
                                             exclude_outcome1==0&
                                             exclude_outcome2==1)]]$ID))))
  print(paste0("# of patients: ", 
               length(unique(d_tmp[d_tmp[,(exclude_ID==0&
                                             exclude_AGE==0&
                                             exclude_Indexdate==0&
                                             exclude_outcome1==0&
                                             exclude_outcome2==0)]]$ID))))
  
  csv_file_name <- paste0(target_folder_path,o,"_clean.csv")
  fwrite(d_tmp, file = csv_file_name, row.names = FALSE)
}

print(paste0("# of patients: ", length(unique(dt_merge$ID))))

length(unique(outcome_list$EyeComp$ID)) 