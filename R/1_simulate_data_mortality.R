

# simulate death event ----
# the simplest would be season-less poisson
# but should be flexible enough to expand
# for daily, a big county has around 25 deaths
# county03 has 10, county54 has 6



simulate_daily_death_event <- function(start_date,
                                       end_date,
                                       model,
                                       param_list){


  # in the future, add season argument
  set.seed(1)

  # start_date <- as.Date("2018-01-01")
  # end_date <- as.Date("2019-12-31")

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














