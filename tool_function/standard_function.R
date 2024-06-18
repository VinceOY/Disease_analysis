#===============================================================================
# standard function sets
# function1: standardized_date()
standardized_date <- function(dt, date_col){
  # Transfer date type 
  if (!grepl("^\\d{4}\\d{2}\\d{2}$", dt[[date_col]][1])) {
    dt[, (date_col) := str_pad(get(date_col), width = 7, side = "left",
                                     pad = "0")]
    years <- substr(dt[[date_col]], 1, 3)
    years_ad <- as.numeric(years) + 1911
    dt[[date_col]] <- paste0(years_ad, "-", substr(dt[[date_col]], 4, 5), 
                             "-", substr(dt[[date_col]], 6, 7))
    dt[[date_col]] <- as.Date(dt[[date_col]], format = "%Y-%m-%d")
  }else {
    dt[[date_col]] <- as.Date(dt[[date_col]], format = "%Y%m%d")
  }
  return(dt)
}
