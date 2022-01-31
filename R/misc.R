# isoyear_c_temp
# @param date The date of interest

isoyear_c_temp <- function(date = lubridate::today()) {
  yr <- format.Date(date, "%G")
  return(yr)
}



# isoyear_n_temp
# @param date The date of interest

isoyear_n_temp <- function(date = lubridate::today()) {
  yr <- as.numeric(isoyear_c_temp(date))
  return(yr)
}

# isoweek_c_temp
# @param date The date of interest

isoweek_c_temp <- function(date = lubridate::today()) {
  # wk <- data.table::isoweek(date)
  # wk <- formatC(wk, flag = "0", width = 2)
  wk <- format.Date(date, "%V")
  return(wk)
}

# isoweek_n_temp
# @param date The date of interest

isoweek_n_temp <- function(date = lubridate::today()) {
  wk <- as.numeric(isoweek_c_temp(date))
  return(wk)
}



# isoyearweek_temp
# @param date The date of interest

isoyearweek_temp <- function(date = lubridate::today()) {
  return(sprintf("%s-%s", isoyear_n_temp(date), isoweek_c_temp(date)))
}

