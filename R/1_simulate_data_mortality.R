# simulate death event ----

#' Simulate daily death event
#'
#' @description
#' Simulate death event on a daily basis in the given time period,
#'   according to user-specified distribution and parameters.
#'
#' This function aims to simulate deaths event when they took place, not when
#'   they were registered. Each data point contains an ID and a date (date of event).
#'
#' The simulated data is typically used for aggregation into different time granularities
#'   such as weekly or quarterly, for further modelling tasks.
#'
#' @param start_date Starting date of the simulation period.
#'   Date is in the format of 'yyyy-mm-dd'.
#'
#' @param end_date Ending date of the simulation period.
#'
#' @param model Model to simulate number of death event on each day from.
#'   Two models have been included: Poisson model and Gaussian approximation model.
#'   See details.
#'
#'   For Poisson model, \code{model = 'poisson'}.
#'
#'   For Gaussian approximation model, \code{model = 'norm_approx'}.
#'
#' @param param_list List of parameters to control the simulation.
#'
#'   For Poisson model, \code{param_list = list(lambda = lambda)}.
#'
#'   For Gaussian model, \code{param_list = list(mu = mu, sigma = sigma)}.
#'
#' @details
#'
#' This function simulates daily death event for an arbitrary location and time period.
#'   Given a starting and ending date, it first generates number of events on each
#'   day, based on a probability distribution or a generating function.
#'
#' Generally, the user needs to specify the expected number of daily event (and
#'   standard deviation, when appropriate).
#'
#' For Poisson model, the parameter is \eqn{\lambda}.
#'
#' For Gaussian model (with approximation), the parameters are \eqn{\mu} and \eqn{\sigma}.
#'   Note that the generated numbers are rounded to the nearest integer and
#'   truncated at zero.
#'
#'
#'
#' @section Future implementations:
#'
#' For simulating daily death event directly from distributions, we plan to include
#'   negative binomial model as an extension to the Poisson model.
#'
#' We also plan to include the simulation with seasonality and trend.
#'
#'
#' @return
#' A data.table containing
#'
#' \describe{
#'   \item{id}{Pseudo ID for death events.}
#'   \item{date}{Date of the death event when it took place.}
#' }
#'
#'
#' @export
#' @examples
#' start_date <- '2018-01-01'
#' end_date <- '2019-12-31'
#'
#' # poisson model with mean daily death 25
#' death_event  <- simulate_daily_death_event(
#'   start_date = start_date,
#'   end_date = end_date,
#'   model = 'poisson',
#'   param_list = list(lambda = 25))
#'
#' # Gaussian model (approximation) with mean daily death 25, standard deviation 2
#' death_event <- simulate_daily_death_event(
#'   start_date = start_date,
#'   end_date = end_date,
#'   model = 'norm_approx',
#'   param_list = list(mu = 25, sigma = 2))

simulate_daily_death_event <- function(start_date,
                                       end_date,
                                       model,
                                       param_list){


  # in the future, add season argument
  set.seed(1)

  # start_date <- '2018-01-01'
  # end_date <- '2019-12-31'

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # model <- 'poisson'
  # param_list <- list(lambda = 25)

  # model <- 'norm_approx'
  # param_list <- list(mu = 25, sigma = 2)


  if(is.null(model)){stop("specify model: poisson, norm_approx, negbin")}
  if(is.null(param_list)){stop("specify param_list, need to match model")}




  dates <- seq.Date(from = start_date, to = end_date, by = 1)
  n_days <- length(dates)


  # generate daily event based on model
  if(model == 'poisson'){
    stopifnot('lambda' %in% names(param_list))

    n <- gen_int_daily_death_event_poi(
      n = n_days,
      lambda = param_list$lambda)

  }else if(model == 'norm_approx'){
    stopifnot('mu' %in% names(param_list))
    stopifnot('sigma' %in% names(param_list))

    n <- gen_int_daily_death_event_norm_approx(
      n = n_days,
      mu = param_list$mu,
      sigma = param_list$sigma)


  }else if(model == 'negbin'){
    # pending


  }else{
    stop("specify model and corresponding parameters")
  }

  # bind daily deaths onto dates
  # repeat each date x times
  # e.g. 2018-01-01, 2018-01-01 means 2 persons died on this day
  # attach pseudo id
  dates_long <- rep(dates, n)

  dt <- data.table::data.table(id = as.character(1:length(dates_long)),
                   date = dates_long)
  return(dt)


}


# simulate registration delay ----

#' Simulate registration date for death event
#'
#' @description
#' Simulate registration date for daily death event, with a prespecified reporting
#'   delay pattern.
#'
#' For now the reporting delay follows a negative binomial distribution.
#'
#' @param death_data A simulated data object from \code{simulate_daily_death_event()}.
#'
#' @param r,p Parameters for negative binomial distribution. Used jointly to control
#'   the mean and variance of the registration delay in terms of days. See detail.
#'
#'   By default, \code{r = 10, p = 0.5}.
#'
#' @details
#' The input data should contain daily individual level death event with date of event.
#'   The number of days for registration (or reporting) delay is then simulated based
#'   on the negative binomial distribution with parameters (r, p).
#'
#' This choice is made by investigating the delay pattern in the real data, which is
#'   often right skewed and has a long tail. NB model is preferable compared to
#'   Poisson model for its flexibility in capturing the over-dispersion.
#'
#' This parametrisation is consistent with \code{stats::rnbinom(n, size = r, prob = p)}.
#'
#' The expected days of delay is then r(1-p)/p. The variance is r(1-p)/p^2.
#'
#' Plausible values are \code{r = 10} and \code{0.5 <= p <= 0.9}, as it is common to
#'   to observe an average delay between 1 to 10 days. However users should adjust
#'   the parameters to examine different delay patterns.
#'
#'
#' @section Future implementations:
#' It is possible to include heavy-tailed distributions.
#'
#' It might be realistic to adjust the delay with varying temporal effect (e.g. weekend).
#'
#' @return
#' A data.table containing
#'
#' \describe{
#'   \item{id}{Pseudo ID for death events.}
#'   \item{date}{Date of the death event when it took place (DOE).}
#'   \item{delay_days}{Delay in days.}
#'   \item{date_reg}{Date of registration (DOR).}
#' }
#'
#'
#' @export
#' @examples
#' start_date <- '2018-01-01'
#' end_date <- '2019-12-31'
#'
#' # simulate death data using the poisson model, with mean daily death 25
#' death_event <- simulate_daily_death_event(
#'   start_date = start_date,
#'   end_date = end_date,
#'   model = 'poisson',
#'   param_list = list(lambda = 25))
#'
#' # simulate delay, with expected delay of 10 days (default)
#' death_event_register <- simulate_registration(death_data = death_event)
#'
#' # different parameters
#' death_event_register <- simulate_registration(death_data = death_event, r = 10, p = 0.7)

simulate_registration <- function(death_data, r = NULL, p = NULL){

  delay_days <- NULL
  date_reg <- NULL

  # death_data <- death_sim
  # simulate individual delay (days) of registration
  # default: 10 days on average

  if(is.null(r)){r <- 10}
  if(is.null(p)){p <- 0.5}

  del_days <- gen_int_daily_delay_nb(n = nrow(death_data),
                                       r = r, p = p)

  # attach
  d <- copy(death_data)
  d[, delay_days := del_days]
  d[, date_reg := as.Date(date + delay_days)]

  return(d)

}





# _________ ----
# internal models -----

gen_int_daily_death_event_nb <- function(n, r, p){

  # x1 <- rnbinom(n = 1000, size = 10, prob = 0.4)
  # mean(x1)
  # var(x1)

  # size: number of success trials (dispersion parameter, shape in gamma mixture)
  # prob: probability of success in each trial
  # gamma(shape = size, scale = (1-prob)/prob)

  # write q = 1-p (reverse)
  # mean: rq/(1-q)
  # var: rq/(1-q)^2

  n_death_daily <- stats::rnbinom(n = n, size = r, prob = p)
  return(n_death_daily)
}


gen_int_daily_death_event_poi <- function(n, lambda){
  # internal, not exported

  n_death_daily <- stats::rpois(n = n, lambda = lambda)
  return(n_death_daily)
}


gen_int_daily_death_event_norm_approx <- function(n, mu, sigma){
  # internal, not exported
  # normal approximation

  n_death_daily <- stats::rnorm(n = n, mean = mu, sd = sigma)
  n_death_daily <- round(n_death_daily, digits = 0)

  # can not be less than 0
  n_death_daily[n_death_daily < 0] <- 0

  return(n_death_daily)
}







# delay

gen_int_daily_delay_nb <- function(n, r, p){

  # the delay structure looks like nb rather than poisson
  # some have very long tails, but in the simulation we don't need them

  # default: r = 10, p = 0.5 gives mean 10
  # other plausible values of p are between 0.4 to 0.9

  delay_vec <- stats::rnbinom(n = n, size = r, prob = p)
  # rnbinom(n = 1000, size = 10, prob = 0.5) %>% hist

  return(delay_vec)
}










