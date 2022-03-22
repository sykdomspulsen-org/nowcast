
# renovate this


nowcast_main <- function(
  data_aggregated,
  offset,
  n_week_adjusting,
  n_week_training,
  date_0,
  nowcast_correction_fn = nowcast_correction_fn_negbin_mm,
  nowcast_correction_sim_fn = nowcast_correction_sim_neg_bin) {

  data_fake_death_clean <- NULL
  ncor <- NULL
  n_death <- NULL
  temp_variable <- NULL
  yrwk <- NULL
  cut_doe <- NULL
  . <- NULL


  ##### for developing
  # data_aggregated <- as.data.table(data_fake_nowcasting_county_aggregated)
  # data_aggregated <- data_aggregated#[location_code == "county03"]
  # n_week_training <- 50
  # n_week_adjusting <- 4
  # date_0 <- data_aggregated[order(cut_doe)][nrow(data_aggregated)]$cut_doe + 1
  # nowcast_correction_fn<- nowcast_correction_fn_negbin_mm
  # nowcast_correction_sim_fn = nowcast_correction_sim_neg_bin
  # # data_aggregated <- data_aggregated[location_code == "county03"]
  # # nowcast_correction_fn<- nowcast_correction_fn_quasipoisson
  # # nowcast_correction_sim_fn = nowcast_correction_sim_quasipoisson
  # offset = "log(pop)"

  data <- as.data.table(data_aggregated)
  n_week_start <- n_week_training + n_week_adjusting
  date_0 <- as.Date(cut(date_0, "week"))

  # Cleaning
  data <- data[cut_doe >= (date_0 - n_week_start*7) ]
  data <- data[cut_doe < date_0]

  #### corrected n_deaths ---- #
  if (!is.null(offset)){
    nowcast_correction_object <- nowcast_correction_fn(data, n_week_adjusting, offset = offset, date_0)
  } else{
    nowcast_correction_object <- nowcast_correction_fn(data, n_week_adjusting, offset = NULL, date_0)
  }

  data <- nowcast_correction_object$data
  data_sim <- nowcast_correction_sim_fn(nowcast_correction_object, offset = offset, date_0 = date_0)

  #check that all the required variables are there
  # (i.e. that the correction function actually gives reasonable stuff back)

  for ( i in 0:(n_week_adjusting-1)){
    temp <- paste0("ncor0_",i)
    if(! temp %in% colnames(data)){
      stop(glue::glue("nowcast_correction_fn is not returning {temp}"))
    }
  }


  data[, ncor := n_death]

  date_0 <- data[order(cut_doe)][nrow(data)]$cut_doe
  for ( i in 0:(n_week_adjusting-1)){
    date_i <- date_0 - 7*i
    temp <- paste0("ncor0_",i)
    data[, temp_variable := get(temp)]
    data[cut_doe == date_i, ncor:= temp_variable]


  }

  data[,temp_variable:=NULL]


  data[, yrwk:= isoyearweek_temp(cut_doe)]
  data_sim[, yrwk:= isoyearweek_temp(cut_doe)]


  col_order <- c(c("yrwk", "location_code", "n_death", "ncor"),
                 colnames(data)[which(!colnames(data) %in% c("yrwk", "n_death", "ncor", "location_code"))])
  setcolorder(data, col_order)

  date_n_Week_adjusting_start <- date_0 - (n_week_adjusting)*7
  data_sim_clean <- data_sim[cut_doe > date_n_Week_adjusting_start]

  col_order_sim <- c(c("yrwk", "location_code", "n_death", "sim_value"),
                     colnames(data_sim_clean)[which(!colnames(data_sim_clean) %in% c("yrwk", "n_death", "sim_value", "location_code"))])
  setcolorder(data_sim_clean, col_order_sim)

  data_sim_clean <- subset(data_sim_clean, select = c("yrwk", "n_death", "sim_value", "cut_doe", "week", "year", "location_code", "sim_id"))



  q025 <- function(x){
    return(stats::quantile(x, 0.025))
  }
  q975 <- function(x){
    return(stats::quantile(x, 0.975))
  }


  col_names <- colnames(data_sim_clean)
  data.table::setkeyv(data_sim_clean,
                      col_names[!col_names %in% c("sim_value", "sim_id")])

  aggregated_data_sim<- data_sim_clean[, unlist(recursive = FALSE, lapply(.(median = stats::median, q025 = q025, q975 = q975),
                                                                          function(f) lapply(.SD, f))),
                                       by = eval(data.table::key(data_sim_clean)),
                                       .SDcols = c("sim_value")]


  retval <- vector("list")
  retval$data <- data
  retval$data_sim <- data_sim_clean
  retval$data_sim_aggregated <- aggregated_data_sim
  return (retval)
}

