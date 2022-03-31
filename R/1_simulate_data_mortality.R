
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






