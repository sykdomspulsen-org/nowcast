# _____ TO DO: _____ ====
# 1. remove unnecessary for loops
# 2. data processing (shifting, training/test split) take out
# 3.





correction_fn_quasipoisson <- function(data,
                                       week_adjust,
                                       keep_offset = T){


  # for developping
  # data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
  # data <- data[location_code == "county03"]
  # week_adjust <- 6
  # keep_offset <- T
  # plot(data$n_death)

  # pay attention to the NA pattern
  # here n0_1 is set to NA for the last week,
  # n0_2 is NA for the last 2 weeks
  # we might need this as well
  # last time is 2019-12-16


  # weekly data
  # remove the p0_x
  # WHEN USE THE NEW DATA, THIS PART IS NOT NEEDED
  data
  dcopy <- copy(data)
  dcopy <- data.table(dcopy)
  dcopy <- dcopy[, p0_0 := NULL]
  dcopy <- dcopy[, p0_1 := NULL]
  dcopy <- dcopy[, p0_2 := NULL]
  dcopy <- dcopy[, p0_3 := NULL]
  dcopy <- dcopy[, p0_4 := NULL]
  dcopy <- dcopy[, p0_5 := NULL]


  dcopy
  # n0_0: registered in the current week
  # n0_1: reg. in the next 1 week
  # n0_2: reg. in the next 2 week (incl. previous weeks)
  # ...
  # correction (prediction)
  # ncor_0: corrected for the current  (avail for all weeks)
  # ncor_1: corrected for next 1 week (avail until last week)
  # --- this uses n0_0, n0_1
  # --- i.e. registered for the current week (until last)
  # --- and registered for the next 1 week (until last, aka this week)



  # create lag1 for n_reg ----

  # FIX THIS WITH REGEXP
  # 5 is week_adjust-1
  n_cols <- paste0('n0_', 0:5)
  n_lag1_cols <- paste0('n_', 0:5, '_lag1')


  dcopy[, (n_lag1_cols):= lapply(.SD, function(x){dplyr::lag(x, default = 0)}),
              .SDcols = n_cols]



  # correction
  # For each week in week_adjust model a correction
  # data up to 6 weeks ago are training
  # data within 6 weeks are predict (actually here only 4 weeks)
  # why n0_i?? what is this 0?
  date_0 <- data[nrow(data),]$cut_doe
  data_train <- dcopy[cut_doe <= (date_0 - week_adjust*7) ]
  # data_predict <- dcopy[cut_doe > (date_0 - week_adjust*7) ]




  # formula ----
  # split the formula
  fy <- 'n_death'
  fx_trend <- 'year'
  fx_seasonal <- 'sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)'

  # by week of correction:
  # i = 0, n0_0_lag1 + n0_0
  # i = 1, n0_1_lag1 + n0_0 + n0_1
  # i = 2, n0_2_lag1 + n0_0 + n0_1 + n0_2

  fx_week <- purrr::map_chr(0:5, function(x){
    c(
      # lag component specific to this week
      glue::glue('n_{x}_lag1'),

      # n_reg within this week; up to next week; ...
      paste0('n0_', 0:x)
    ) %>%  paste(collapse = '+')
  })


  # paste components together
  ff <- paste0(fy, '~', fx_trend, '+', fx_seasonal, '+', fx_week)

  if(keep_offset == T){
    ff <- paste0(ff,  "+ offset(log(pop))")
  }


  # fit correction ----
  fit_list <- vector(mode = "list", length = (week_adjust))
  ncor_list <- vector(mode = "list", length = (week_adjust))

  for(i in 1:6){
    # i <- 1
    # use training data
    fit <- stats::glm(stats::as.formula(ff[i]),
                      family = "quasipoisson",
                      data = data_train)


    # predict for ith week (rolling forward)
    # it is NOT predicting the test data
    # it is prediction BOTH train and test (correction)
    # for ncor_0 (how many SHOULD be registered this week),
    # it is mostly the temporal trend
    # effect of n0_0, n0_0_lag is minimal


    # SHOULD TRY TO REMOVE THE WARNING
    ncor <- round(stats::predict(fit,
                                 newdata = dcopy, # RENAME it
                                 type = "response"))

    fit_list[[i]] <- fit
    ncor_list[[i]] <- ncor

  }

  # bind the corrections
  ncor_dt <- do.call(cbind, ncor_list) %>% data.table()
  colnames(ncor_dt) <- paste0('ncor_', 0:5)
  ncor_dt


  retval <- vector("list")
  retval$data<- data
  retval$week_adjust <- week_adjust
  retval$fit <- fit_list
  # is formula list needed?

  return(retval)
}












