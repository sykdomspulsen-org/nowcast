# _____ TO DO: _____ ====
# 1.
# 2. data processing (shifting, training/test split) take out
# 3. flexible formula
# 4. argument for week adjust: make a check for processed data week_to_adjust
# e.g. in data there is already expanded columns. use this instead of external




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

  week_id <- 1:week_adjust - 1
  # FIX THIS WITH REGEXP
  # 5 is week_adjust-1
  n_cols <- paste0('n0_', week_id)
  n_lag1_cols <- paste0('n_', week_id, '_lag1')


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

  fx_week <- purrr::map_chr(week_id, function(x){
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

  for(i in 1:week_adjust){
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
  colnames(ncor_dt) <- paste0('ncor_', week_id)
  ncor_dt


  retval <- vector("list")
  retval$data<- data
  retval$week_adjust <- week_adjust
  retval$fit <- fit_list
  retval$n_corrected <- ncor_dt
  # is formula list needed?

  return(retval)
}




corobj_qp <- correction_fn_quasipoisson(data = data_fake_nowcasting_county_aggregated,
                                        week_adjust = 6,
                                        keep_offset = T)



correction_sim_quasipoisson <- function(nowcast_correction_object, offset, n_sim = 500, date_0){
  # only corrects n_week_Adjusting weeks! not +1
  n_death <- NULL
  sim_value <- NULL
  cut_doe <- NULL



  # data <- data.table::as.data.table(data_fake_nowcasting_county_aggregated)[location_code == "county03"]
  # n_week_adjusting <- 5
  # n_week_train <- 52
  # n_week_start <- n_week_adjusting + n_week_train
  # date_0 <- data[nrow(data),]$cut_doe #last date in the dataset, assume the dataset is ordered.
  # data <- data[cut_doe >= (date_0 - n_week_start*7 + 1), ]
  # nowcast_correction_object <- nowcast_correction_fn_quasipoisson(data,
  #    n_week_adjusting, offset = TRUE, date_0 )
  # nowcast_sim <- nowcast_correction_sim_quasipoisson(nowcast_correction_object,
  #      offset = "log(pop)", date_0= date_0)

  fit_vec <- nowcast_correction_object$fit
  data <- nowcast_correction_object$data
  n_week_adjusting <- nowcast_correction_object$n_week_adjusting[[1]]

  # fit_vec[[1]]
  # each fit_vec has fit and formula



  ##simulations ---- #

  data_train <- data[cut_doe < (date_0 - n_week_adjusting*7) ]
  data_predict <- data[cut_doe >= (date_0 - n_week_adjusting*7) ]

  # use DATA_PREDICT here, not test
  cut_doe_vec <- data_predict$cut_doe
  cut_doe_vec_unique <- unique(cut_doe_vec) # ??

  sim_val_vec <- vector("list", length = (n_week_adjusting))

  for ( i in 0:(n_week_adjusting-1)){
  # i <- 0

    # take out the fit and formula
    fit <-fit_vec[[i+1]]$fit
    formula <- fit_vec[[i+1]]$formula
    cut_doe_cur <- cut_doe_vec_unique[n_week_adjusting-i]
    # i is 0, use 5th
    # i is 4, use 1st

    # n_rows <- length(which(cut_doe_cur == cut_doe_vec))

    # posterior dist ----
    x <- arm::sim(fit, n_sim)

    sim_models <- as.data.frame(x@coef)


    data_x <- as.data.table(copy(stats::model.frame(formula, data = data)))


    data_x <- data_x[(nrow(data_x)-n_rows+1):nrow(data_x),]  # ???
    # Coiuld potentially create trouble but should works because al later rows contain NA and are hence remowed.
    data_x[, n_death:= NULL]


    col_names<-  colnames(sim_models)
    col_names_rel <- col_names[which(col_names != "(Intercept)")] # covariates

    if (!"(Intercept)" %in% colnames(sim_models) ){

      # shouldn't it always be false??
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
      # added 1 for the intercept
      # this part should be simplified
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

    # simulate from distribution ----

    dispersion<- summary(fit)$dispersion
    if(dispersion > 1){
      # over-dispersed, from nb
      expected_sim <-data.table(
        sim_id = 1:n_sim,
        sim_value = (stats::rnbinom(length(expected),mu = exp(expected), size = (exp(expected)/(dispersion-1)))),
        cut_doe = cut_doe_cur
      )
    } else{
      # not over-dispersed, from poi
      expected_sim <-data.table(
        sim_id = 1:n_sim,
        sim_value = stats::rpois(length(expected),lambda  = exp(expected)),
        cut_doe = cut_doe_cur
      )
    }

    sim_val_vec[[i +1]]<- expected_sim


  }


  # return every fit
  sim_data <- rbindlist(sim_val_vec)
  retval<- merge(data, sim_data, by = "cut_doe", all = TRUE)
  return(retval)
}






