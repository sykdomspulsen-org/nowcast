# _____ TO DO: _____ ====
# 1. remove unnecessary for loops
# 2. data processing (shifting, training/test split) take out
# 3.





correction_fn_quasipoisson <- function(data, n_week_adjusting, offset, date_0){


  # for developping
  # data<- data.table::as.data.table(data_fake_nowcasting_county_aggregated)
  # data <- data[location_code == "county03"]
  # n_week_adjusting <- 6
  # offset = "log(pop)"
  # date_0 <- data[nrow(data),]$cut_doe
  plot(data$n_death)

  # pay attention to the NA pattern
  # here n0_1 is set to NA for the last week,
  # n0_2 is NA for the last 2 weeks
  # we might need this as well
  # last time is 2019-12-16


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

  # formula ----
  # split the formula
  fy <- 'n_death'
  fx_trend <- 'year'
  fx_seasonal <- 'sin(2 * pi * (week - 1) / 52) + cos(2 * pi * (week - 1) / 52)'
  formula1 <- paste0(fy, '~', fx_trend, '+', fx_seasonal)
  formula1

  # i = 0, n0_0_lag1 + n0_0
  # i = 1, n0_1_lag1 + n0_0 + n0_1
  # i = 2, n0_2_lag1 + n0_0 + n0_1 + n0_2

i <- 0
 # formula_list <- list()
  for ( i in 0:(n_week_adjusting-1)){
    # print(i)
    # i <- 1
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
    # it is NOT predicting the test data
    # it is prediction BOTH train and test (correction)

    n_cor <- round(stats::predict(fit, newdata = data, type = "response"))
    data[, glue::glue("ncor0_{i}"):= n_cor]

    fit_vec[[i+1]]$fit<- fit
    fit_vec[[i+1]]$formula<- formula
  # # formula_list[[i+1]] <- formula
  }




  retval <- vector("list")
  retval$data<- data
  retval$n_week_adjusting <- n_week_adjusting
  retval$fit <- fit_vec



  return(retval)
}












