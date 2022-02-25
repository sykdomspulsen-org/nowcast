
### nowcast_correction_fn_negbin ---- #

#' Nowcast correction function using the negative binomial distribution.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="nowcast")}
#'
#' @param data Data generated with nowcast_aggregate containing the part of the dataset that the model should train on.
#' @param n_week_adjusting Number of weeks to correct
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return nowcast_correction_object including corrected data for all weeks in n_wwk_adjust and the model fits for all weeks
#' @examples \dontrun{
#' data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' n_week_adjusting <- 5
#' n_week_start <- 52
#' date_0 <- data[nrow(data),]$cut_doe + 1 #first date not in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_negbin_mm(data,
#'                            n_week_adjusting, offset = "log(pop)" , date_0)
#' }
#' @export
nowcast_correction_fn_negbin_mm <- function(data, n_week_adjusting, offset, date_0){

  temp_variable_n <- NULL
  cut_doe <- NULL
  . <- NULL
  location_code <- NULL


  ##for developping

  # data<- as.data.table(data_fake_nowcasting_county_aggregated)
  # n_week_adjusting <- 4
  # offset = "log(pop)"
  # date_0 <- data[order(cut_doe)][nrow(data)]$cut_doe +1

  date_0 <- as.Date(cut(date_0, "week"))
  data <- data[cut_doe <= date_0,]

  # Lag the data
  for ( i in 0:(n_week_adjusting-1)){

    week_n <- paste0("n0_",(i))
    data[, temp_variable_n := get(week_n)]
    keycol <-  c("year", "week", "location_code")
    setorderv(data, keycol)
    data[, paste0("n0_",(i), "_lag1") := shift(temp_variable_n, 1, fill = 0), by = .(location_code)] #by = (location_code, data)
  }
  data <- subset(data, select= -c(temp_variable_n))

  data_train <- data[cut_doe <= (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe > (date_0 - n_week_adjusting*7) ]


  # correct
  # fit a model for every week of correction
  fit_vec <- vector(mode = "list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){
    # print(i)
    fixef <- paste0("sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+ year +",
                    glue::glue("n0_{i}"))

    ranef <- "(1| location_code)"

    response <- "n_death"

    # default is poisson, but here use negbin
    fit <- fit_attrib(data_train,
                      response= response,
                      fixef=fixef,
                      ranef=ranef,
                      offset= offset,
                      dist_family = "negbin")

    n_cor <- round(stats::predict(fit, newdata = data, type = "response"))
    data[, glue::glue("ncor0_{i}"):= n_cor]

    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$fixef<- fixef
    fit_vec[[i+1]]$ranef <- ranef
    fit_vec[[i+1]]$response <- response
    fit_vec[[i+1]]$offset <- offset


  }

  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec

  return(retval)
}


### nowcast_correction_sim ---- #


#' Nowcast simmulation function when nowcast_correction_fn_negbin is used for correction.
#'
#' For more details see the help vignette.:
#' \code{vignette("nowcast", package="nowcast")}
#'
#' @param nowcast_correction_object object returned from function nowcast_correction_fn_expanded
#' @param n_sim Number of simulations
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return simulations of the estimate made by the fitted models in nowcast_correction_fn
#' @examples
#' \dontrun{
#' data <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' n_week_adjusting <- 6
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_negbin_mm(data, n_week_adjusting,
#'    offset = "log(pop)" , date_0 = date_0)
#' nowcast_sim <- nowcast_correction_sim_neg_bin(nowcast_correction_object,
#'     offset = "log(pop)", date_0 = date_0)
#' }
#' @export
nowcast_correction_sim_neg_bin <- function(nowcast_correction_object, offset, n_sim = 500, date_0){

  n_death <- NULL
  sim_value <- NULL
  . <- NULL
  sim_id <- NULL
  location_code <- NULL
  cut_doe <- NULL


  # for developping
  # data<- as.data.table(data_fake_nowcasting_county_aggregated)
  # n_week_adjusting <- 4
  # n_sim <- 100
  # date_0 <- data[order(cut_doe)][nrow(data)]$cut_doe
  # nowcast_correction_object<- nowcast_correction_fn_negbin_mm(data, n_week_adjusting, offset = "log(pop)", date_0)
  # offset <- "log(pop)"

  fit_vec <- nowcast_correction_object$fit
  data <- nowcast_correction_object$data
  n_week_adjusting <- nowcast_correction_object$n_week_adjusting[[1]]

  ##simulations ---- #

  data_train <- data[cut_doe <= (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe >= (date_0 - n_week_adjusting*7) ]

  cut_doe_vec <- data_predict$cut_doe
  cut_doe_vec_unique <- unique(cut_doe_vec)

  sim_val_vec <- vector("list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){


    fit <-fit_vec[[i+1]]$fit
    formula <- fit_vec[[i+1]]$formula
    cut_doe_cur <- cut_doe_vec_unique[n_week_adjusting-i]
    n_row <- length(which(cut_doe_cur == cut_doe_vec))

    # customary sim (still requires arm::sim)
    sim_data<- sim(fit, data_predict[cut_doe== cut_doe_cur], n_sim = n_sim)

    shape<- lme4::getME(fit, "glmer.nb.theta")
    sim_data[, sim_value := stats::rnbinom(.N, shape, mu = sim_value)]


    expected_sim <-sim_data[,.(cut_doe, sim_id, sim_value, location_code)]
    sim_val_vec[[i +1]]<- expected_sim


  }


  sim_data <- rbindlist(sim_val_vec)
  retval<- merge(data, sim_data, by = c("cut_doe", "location_code"), all = TRUE)
  return(retval)
}


