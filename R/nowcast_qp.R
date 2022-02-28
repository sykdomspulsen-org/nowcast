
### nowcast_correction_fn_quasipoisson ---- #

#' Nowcast correction function using the quasipoisson distribution.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="nowcast")}
#'
#' @param data Data generated with nowcast_aggregate containing the part of the dataset that the model should train on.
#' @param n_week_adjusting Number of weeks to correct
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return nowcast_correction_object including corrected data for all weeks in n_wwk_adjust and the model fits for all weeks
#' @examples
#' data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
#' data <- data[location_code == "county03"]
#' n_week_adjusting <- 5
#' n_week_train <- 52
#' n_week_start <- n_week_adjusting + n_week_train
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_quasipoisson(data,
#'      n_week_adjusting, offset = "log(pop)", date_0 )
#' @export
nowcast_correction_fn_quasipoisson <- function(data, n_week_adjusting, offset, date_0){

  temp_variable_n <- NULL
  cut_doe <- NULL
  . <- NULL
  location_code <- NULL

  # for developping
  # data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
  # data <- data[location_code == "county03"]
  # n_week_adjusting <- 6
  # offset = "log(pop)"

  # weekly data

  # Lag the data
  # does not need a for loop
  # created a one-week lag in n (no p)
  for ( i in 0:(n_week_adjusting-1)){

    week_n <- paste0("n0_",(i))
    data[, temp_variable_n := get(week_n)]
    keycol <-  c("year", "week", "location_code")
    setorderv(data, keycol)
    data[, paste0("n0_",(i), "_lag1") := shift(temp_variable_n, 1, fill = 0), by = .(location_code)] #by = (location_code, data)
  }
  data <- subset(data, select= -c(temp_variable_n))


  # correction
  # For each week in n_week_adjusting model a correction
  # data up to 6 weeks ago are training
  # data within 6 weeks are predict (actually here only 4 weeks)
  # why n0_i?? what is this 0?

  data_train <- data[cut_doe <= (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe > (date_0 - n_week_adjusting*7) ]

  fit_vec <- vector(mode = "list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){
    # print(i)
    formula <- paste0("n_death", "~sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)+ year +",
                      glue::glue("n0_{i}_lag1"), "+",  glue::glue("n0_{i}"))
    if(i>=1){
      for (j in 0:(i-1)){
        formula <-  paste0(formula, "+",  glue::glue("n0_{j}"))
      }
    }

    if(!is.null(offset)){
      formula <- paste0(formula,  "+ offset(log(pop))")
    }


    fit <- stats::glm(stats::as.formula(formula), family = "quasipoisson", data = data_train)


    # predict for ith week (rolling forward)
    n_cor <- round(stats::predict(fit, newdata = data, type = "response"))
    data[, glue::glue("ncor0_{i}"):= n_cor]

    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$formula<- formula

  }

  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec

  # _____ TO DO: _____ ====
  # 1. remove unnecessary for loops
  # 2. data processing (shifting, training/test split) take out
  # 3.



  return(retval)
}


# simulation -----

### nowcast_correction_sim ---- #

#' Nowcast simmulation function when nowcast_correction_fn_quasipoisson is used for correction.
#'
#' For more details see the help vignette:
#' \code{vignette("nowcast", package="nowcast")}
#'
#' @param nowcast_correction_object object returned from function nowcast_correction_fn_expanded
#' @param n_sim Number of simulations
#' @param offset Boolian value which is set to true if offset(log(pop)) is a part of the formula
#' @param date_0 Date of aggregation.
#' @return simulations of the estimate made by the fitted models in nowcast_correction_fn
#' @examples
#' data <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)[location_code == "county03"]
#' n_week_adjusting <- 5
#' n_week_train <- 52
#' n_week_start <- n_week_adjusting + n_week_train
#' date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
#' data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
#' nowcast_correction_object <- nowcast_correction_fn_quasipoisson(data,
#'    n_week_adjusting, offset = TRUE, date_0 )
#' nowcast_sim <- nowcast_correction_sim_quasipoisson(nowcast_correction_object,
#'      offset = "log(pop)", date_0= date_0)
#' @export
nowcast_correction_sim_quasipoisson <- function(nowcast_correction_object, offset, n_sim = 500, date_0){
  # only corrects n_week_Adjusting weeks! not +1
  n_death <- NULL
  sim_value <- NULL
  cut_doe <- NULL


  # for developping
  # data<- as.data.table(data_fake_nowcasting_county_aggregated)[location_code == "county03"]
  # n_week_adjusting <- 4
  # n_sim <- 100
  # nowcast_correction_object<- nowcast_correction_fn_quasipoisson(data, n_week_adjusting, offset = "log(pop)")
  # offset <- "log(pop)"

  fit_vec <- nowcast_correction_object$fit
  data <- nowcast_correction_object$data
  n_week_adjusting <- nowcast_correction_object$n_week_adjusting[[1]]

  ##simulations ---- #

  data_train <- data[cut_doe < (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe >= (date_0 - n_week_adjusting*7) ]

  cut_doe_vec <- data_predict$cut_doe
  cut_doe_vec_unique <- unique(cut_doe_vec)

  sim_val_vec <- vector("list", length = (n_week_adjusting))
  for ( i in 0:(n_week_adjusting-1)){


    fit <-fit_vec[[i+1]]$fit
    formula <- fit_vec[[i+1]]$formula
    cut_doe_cur <- cut_doe_vec_unique[n_week_adjusting-i]
    n_rows <- length(which(cut_doe_cur == cut_doe_vec))

    # posterior dist
    x<- arm::sim(fit, n_sim)
    sim_models <- as.data.frame(x@coef)
    data_x <- as.data.table(copy(stats::model.frame(formula, data = data)))
    data_x <- data_x[(nrow(data_x)-n_rows+1):nrow(data_x),] # Coiuld potentially create trouble but should works because al later rows contain NA and are hence remowed.
    data_x[, n_death:= NULL]


    col_names<-  colnames(sim_models)
    col_names_rel <- col_names[which(col_names != "(Intercept)")]
    if (!"(Intercept)" %in% colnames(sim_models) ){
      if(!is.null(offset)){
        colnames(cbind(sim_models, 1))
        rownames(rbind( as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models, 1)) %*%  rbind(as.matrix(t(data_x)))

      }else{
        colnames(cbind(sim_models))
        rownames(rbind( as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models)) %*%  rbind(as.matrix(t(data_x)))
      }
    } else{
      if(!is.null(offset)){
        colnames(cbind(sim_models, 1))
        rownames(rbind(1, as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models, 1)) %*%  rbind(1,as.matrix(t(data_x)))
      }else{
        colnames(cbind(sim_models))
        rownames(rbind(1, as.matrix(t(data_x))))

        expected <- as.matrix(cbind(sim_models)) %*%  rbind(1,as.matrix(t(data_x)))
      }
    }

    dispersion<- summary(fit)$dispersion
    if(dispersion > 1){
      expected_sim <-data.table(
        sim_id = 1:n_sim,
        sim_value = (stats::rnbinom(length(expected),mu = exp(expected), size = (exp(expected)/(dispersion-1)))),
        cut_doe = cut_doe_cur
      )
    } else{
      expected_sim <-data.table(
        sim_id = 1:n_sim,
        sim_value = stats::rpois(length(expected),lambda  = exp(expected)),
        cut_doe = cut_doe_cur
      )
    }

    sim_val_vec[[i +1]]<- expected_sim


  }

  sim_data <- rbindlist(sim_val_vec)
  retval<- merge(data, sim_data, by = "cut_doe", all = TRUE)
  return(retval)
}
