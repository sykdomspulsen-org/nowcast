
# nowcast main * ----
#' Nowcast, corrigation of data
#'
#' When there is a lag in the reception of data nowcast can be used to correct for this lag,
#'  and predict the true underlying values.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="attrib")}
#'
#' @param data_aggregated Aggregated dataset from the function npowcast_aggregate
#' @param offset offset
#' @param n_week_adjusting Total number of weeks to correct.
#' @param n_week_training Number of weeks to train on
#' @param date_0 Date of aggregation.
#' @param nowcast_correction_fn Correction function. The deafault is nowcast_correction_fn_expanded. Must return the same as this function.
#' @param nowcast_correction_sim_fn Simmulatoin function. Must return a datatable with the following collumns  "n_death", "sim_value", "cut_doe", "ncor" and simmulations for equally many weeks as n_week_adjust.
#' @examples
#'
#'data_aggregated <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#'n_week_training <- 50
#'n_week_adjusting <- 4
#'date_0 <- data_aggregated[order(cut_doe)][nrow(data_aggregated)]$cut_doe + 1
#'offset = "log(pop)"
#'nowcast_object <- nowcast(data_aggregated,offset,n_week_adjusting,n_week_training,date_0)
#'
#' @examples
#'
#'data_aggregated <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#'data_aggregated <- data_aggregated[location_code == "county03"]
#'n_week_training <- 50
#'n_week_adjusting <- 4
#'date_0 <- data_aggregated[order(cut_doe)][nrow(data_aggregated)]$cut_doe + 1
#'offset = "log(pop)"
#'nowcast_object <- nowcast(data_aggregated,offset,n_week_adjusting,n_week_training,date_0,
#' nowcast_correction_fn = nowcast_correction_fn_quasipoisson,
#' nowcast_correction_sim_fn = nowcast_correction_sim_quasipoisson)
#' @return Dataset including the corrected values for n_death
#'
#' @export
nowcast <- function(
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




#' Evaluation of the nowcast performance.
#'
#' Evaluates the performance of the nowcast function on historic data.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="nowcast")}
#'
#' @param nowcast_object Object from the function nowcast
#' @param n_week_adjusting Number of weeks to adjust
#' @examples
#'
#'  data_aggregated <-  data.table::as.data.table(data_fake_nowcasting_county_aggregated)[location_code == "county03",]
#'  n_week_training <- 50
#'  n_week_adjusting <- 5
#'  date_0 <- data_aggregated[nrow(data_aggregated),]$cut_doe
#'  nowcast_object <- nowcast(data_aggregated,offset = "log(pop)",
#'   n_week_adjusting,n_week_training,date_0,
#'   nowcast_correction_fn = nowcast_correction_fn_quasipoisson,
#'   nowcast_correction_sim_fn = nowcast_correction_sim_quasipoisson)
#'  nowcast_eval_object <- nowcast_eval(nowcast_object, n_week_adjusting)
#' @return Residualplots for all ncor_i and some evaluationmetrixs for each of them as well as a plot containing credible intervals using the simulations
#' @export
#'
nowcast_eval <- function(nowcast_object, n_week_adjusting){


  temp_variable <- NULL
  residual <- NULL
  n_death <- NULL
  std_residual <- NULL
  diff_n_death_mean <- NULL
  na.omit <- NULL
  data_fake_nowcasting_aggregated <- NULL
  diff_n_death_mean <- NULL
  q05 <- NULL
  q95 <- NULL
  . <- NULL
  median <- NULL
  yrwk <- NULL
  median.sim_value <- NULL
  q025.sim_value <- NULL
  q975.sim_value <- NULL


  ### for developing

  # data_aggregated <- as.data.table(data_fake_nowcasting_county_aggregated)
  # n_week_training <- 50
  # n_week_adjusting <- 4
  # nowcast_object <- nowcast_exp(data_aggregated= data_aggregated,
  #                               n_week_training = n_week_training,
  #                               n_week_adjusting = n_week_adjusting,
  #                               offset = "log(pop)")



  data <- nowcast_object$data
  data_sim <- nowcast_object$data_sim
  data_aggregated <- nowcast_object$data_sim_aggregated

  retval <- vector("list" , length = (n_week_adjusting))

  # Compute R2, mse, residualplot for every correction

  # numbers of rows with corrected data
  n_loc <- length(unique(data$location_code))
  n_row_corrections <- n_loc *n_week_adjusting
  for (i in 0:(n_week_adjusting-1) ){
    temp <- paste0("ncor0_", i)
    data[, temp_variable := get(temp)]
    data[, residual:= temp_variable -n_death]
    std <- (sum(data$residual[(nrow(data)- n_row_corrections +1):(nrow(data)- n_loc*i)]**2))**0.5 #this is rediculous!!! totally wrong
    data[, std_residual:= (temp_variable -n_death)/std]

    mean <- sum(data$n_death)/nrow(data)
    data[, diff_n_death_mean := n_death - mean]

    R2 <- 1- (sum(na.omit(data)$residual**2))/(sum(na.omit(data)$diff_n_death_mean**2))
    MSE <- sum(na.omit(data)$residual**2)/nrow(na.omit(data))
    q <- ggplot2::ggplot(data, ggplot2::aes(x = temp_variable, y = std_residual))
    q <- q + ggplot2::geom_point()
    q <- q + ggplot2::geom_hline(yintercept = 0, colour = "red")

    q <- q + ggplot2::scale_y_continuous("Standard residuals")
    q <- q + ggplot2::scale_x_continuous("Number of deaths")

    q <- q + ggplot2::labs(caption = glue::glue(" {lubridate::today()}"))
    q <- q + ggplot2::ggtitle(paste( "Stdandard residuals for", temp))
    q
    temp_retval <- list()
    temp_retval$ncor <- i
    temp_retval$std_residualplot <- copy(q)

    # q <- ggplot2::ggplot(data, ggplot2::aes(x = temp_variable, y = residual))
    # q <- q + ggplot2::geom_point()
    # q <- q + ggplot2::geom_hline(yintercept = 0, colour = "red")
    # q <- q + ggplot2::ggtitle(temp)
    # temp_retval$residualplot <- copy(q)

    abs_error <- sum(abs(na.omit(data$residual)))/nrow(na.omit(data))
    temp_retval$abs_error <- abs_error
    temp_retval$R_squared <- R2
    temp_retval$MSE <- MSE
    temp_retval$RMSE <- MSE**0.5

    retval[[i +1]] <- temp_retval
  }

  return (retval)
}

