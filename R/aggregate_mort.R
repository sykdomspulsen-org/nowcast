#' Cleaned fake data for mortality registration
#'
#' @format
#' \describe{
#' \item{cut_doe}{First date of every week}
#' \item{n_death}{Number of true deaths this week}
#' \item{n0_0}{Number of registrations within the current week}
#' \item{n0_1}{Number of registrations within the current and previous week}
#' \item{p0_1}{Percentile of registrations within the current and previous week}
#' \item{n0_2}{Number of registrations within the 2 last weeks and the current week}
#' \item{p0_2}{Percentile of registrations within the current and prvious 2 weeks}
#' \item{n0_3}{Number of registrations within the 3 last weeks and the current week}
#' \item{p0_3}{Percentile of registrations within the current and prvious 3 weeks}
#' \item{n0_4}{Number of registrations within the 4 weeks and the current week}
#' \item{p0_4}{Percentile of registrations within the current and prvious 4 weeks}
#' \item{n0_5}{Number of registrations within the 5 weeks and the current week}
#' \item{p0_5}{Percentile of registrations within the current and prvious 5 weeks}
#' \item{n0_6}{Number of registrations within the 6 weeks and the current week}
#' \item{p0_6}{Percentile of registrations within the current and prvious 6 weeks}
#' \item{n0_7}{Number of registrations within the 7 weeks and the current week}
#' \item{p0_7}{Percentile of registrations within the current and prvious 7 weeks}
#' \item{n0_8}{Number of registrations within the 8 weeks and the current week}
#' \item{p0_8}{Percentile of registrations within the current and prvious 8 weeks}
#' \item{n0_9}{Number of registrations within the 9 weeks and the current week}
#' \item{p0_9}{Percentile of registrations within the current and prvious 9 weeks}
#' \item{n0_10}{Number of registrations within the 10 weeks and the current week}
#' \item{p0_10}{Percentile of registrations within the current and prvious 10 weeks}
#' \item{n0_11}{Number of registrations within the 11 weeks and the current week}
#' \item{p0_11}{Percentile of registrations within the current and prvious 11 weeks}
#' \item{n0_12}{Number of registrations within the 12 weeks and the current week}
#' \item{p0_12}{Percentile of registrations within the current and prvious 12 weeks}
#' \item{n0_13}{Number of registrations within the 13 weeks and the current week}
#' \item{p0_13}{Percentile of registrations within the current and prvious 13 weeks}
#' \item{n0_14}{Number of registrations within the 14 weeks and the current week}
#' \item{p0_14}{Percentile of registrations within the current and prvious 14 weeks}
#' \item{n0_15}{Number of registrations within the 15 weeks and the current week}
#' \item{p0_15}{Percentile of registrations within the current and prvious 15 weeks}
#' }
"data_fake_nowcasting_aggregated"

#' Cleaned fake data for mortality registration on a county basis
#'
#' @format
#' \describe{
#' \item{cut_doe}{First date of every week}
#' \item{location_code}{Location code}
#' \item{n_death}{Number of true deaths this week}
#' \item{n0_0}{Number of registrations within the current week}
#' \item{n0_1}{Number of registrations within the current and previous week}
#' \item{p0_1}{Percentile of registrations within the current and previous week}
#' \item{n0_2}{Number of registrations within the 2 last weeks and the current week}
#' \item{p0_2}{Percentile of registrations within the current and prvious 2 weeks}
#' \item{n0_3}{Number of registrations within the 3 last weeks and the current week}
#' \item{p0_3}{Percentile of registrations within the current and prvious 3 weeks}
#' \item{n0_4}{Number of registrations within the 4 weeks and the current week}
#' \item{p0_4}{Percentile of registrations within the current and prvious 4 weeks}
#' \item{n0_5}{Number of registrations within the 5 weeks and the current week}
#' \item{p0_5}{Percentile of registrations within the current and prvious 5 weeks}
#' \item{n0_6}{Number of registrations within the 6 weeks and the current week}
#' \item{p0_6}{Percentile of registrations within the current and prvious 6 weeks}
#' \item{n0_7}{Number of registrations within the 7 weeks and the current week}
#' \item{p0_7}{Percentile of registrations within the current and prvious 7 weeks}
#' \item{n0_8}{Number of registrations within the 8 weeks and the current week}
#' \item{p0_8}{Percentile of registrations within the current and prvious 8 weeks}
#' \item{n0_9}{Number of registrations within the 9 weeks and the current week}
#' \item{p0_9}{Percentile of registrations within the current and prvious 9 weeks}
#' \item{n0_10}{Number of registrations within the 10 weeks and the current week}
#' \item{p0_10}{Percentile of registrations within the current and prvious 10 weeks}
#' \item{n0_11}{Number of registrations within the 11 weeks and the current week}
#' \item{p0_11}{Percentile of registrations within the current and prvious 11 weeks}
#' \item{n0_12}{Number of registrations within the 12 weeks and the current week}
#' \item{p0_12}{Percentile of registrations within the current and prvious 12 weeks}
#' \item{n0_13}{Number of registrations within the 13 weeks and the current week}
#' \item{p0_13}{Percentile of registrations within the current and prvious 13 weeks}
#' \item{n0_14}{Number of registrations within the 14 weeks and the current week}
#' \item{p0_14}{Percentile of registrations within the current and prvious 14 weeks}
#' \item{n0_15}{Number of registrations within the 15 weeks and the current week}
#' \item{p0_15}{Percentile of registrations within the current and prvious 15 weeks}
#' }
"data_fake_nowcasting_county_aggregated"










#' Aggregation of data for nowcasting
#'
#' Aggregates mortality data to a weekly basis.
#' Where the percentiles and number of deaths obtained after every week up to n_week is also given.
#' For more details see the help vignette:
#'
#' \code{vignette("intro", package="nowcast")}
#'
#' @param data Dataset containing doe (Date of event), dor (Date of registation) and location_code. The columns must have these exact names.
#' @param aggregation_date Date of aggregation
#' @param n_week Number of weeks to calculate the percentage of the total registraations. Must be larger og equal to 2 amd smaller than the total number of weeks in the dataset.
#' @param pop_data Population data, must contain a column called pop with the population data and a column with year and possibly week.
#' @examples
#'
#' data <- nowcast::data_fake_nowcasting_raw
#' data[doe < as.Date("2019-01-01")]
#' aggregation_date <- as.Date("2020-01-01")
#' n_week <- 6
#'
#' data_aggregated <- nowcast_aggregate(data, aggregation_date, n_week)
#'
#' @return Aggregated dataset with the percentiles of registered events within the last 52 weeks
#'
#' @export
nowcast_aggregate <- function(
  data,
  aggregation_date,
  n_week,
  pop_data = NULL) {

  doe <- NULL
  dor <- NULL
  cut_doe <- NULL
  n_death <- NULL
  temp_outcome <- NULL
  n_death_registered <- NULL
  p_death_registered <- NULL
  n0_0 <- NULL
  p0_0 <- NULL
  temp_variable_n <- NULL
  temp_variable_p <- NULL
  . <- NULL
  new_value <- NULL
  temp_outcome_n <- NULL
  temp_outcome_p<- NULL
  pop <- NULL
  location_code<- NULL


  ##### for developing

  # data <- nowcast::data_fake_nowcasting_raw
  # aggregation_date <- as.Date("2019-12-31")
  # n_week <- 6
  # pop_data <- fhidata::norway_population_by_age_cats(cats = list(c(1:120)))[location_code %in% unique(fhidata::norway_locations_b2020$county_code)]



  # check of parameters ----

  if (! "doe" %in% colnames(data)){
    stop("The dataset does not have the correct column names, doe is missing")
  }

  if (! "dor" %in% colnames(data)){
    stop("The dataset does not have the correct column names, dor is missing")
  }
  if (! "location_code" %in% colnames(data)){
    stop("The dataset does not have the correct column names, location_code is missing")
  }

  if (! "n_week" > 1){
    stop("n_week is to small" )
  }


  d <- data.table(data)
  d <- d[, .(doe, dor, location_code)]


  # count weekly total ----
  # cut(aggregation_date + 1:7, 'day')
  # cut(aggregation_date + 1:7, 'week') # 2 levels (weeks)
  # fhidata::days[mon == '2020-01-06']

  # only keep part before aggregation date
  # convert aggre_date into monday of that week
  # e.g. 2020-12-31 is a tuesday, so it'll be converted to 12-30 (monday)


  d <- d[dor < as.Date(cut(aggregation_date, "week"))]
  d <- d[doe < as.Date(cut(aggregation_date, "week"))]
  d[, cut_doe := as.Date(cut(doe, "week"))]

  # cut_doe is the monday of that week
  # fhidata::days[mon == '2018-01-01']
  # fhidata::days[mon == '2019-12-23']
  # could be useful to attach isoyearweek too

  d <- d[order(doe, dor)]


  # count total deaths (ever registered)
  # per week per location
  d_death_week_location <- d[ , .(
    "n_death" = .N
  ), keyby = .(
    cut_doe,
    location_code
  )]

  # combine with dor
  d[ d_death_week_location,
     on = c("cut_doe","location_code"),
     n_death := n_death]

  retval <- vector("list", length = n_week)
  # d_within_week <- d[, .(cut_doe, location_code)]




  # count weekly registered ----
  # n_week is the weeks to ??
  n_week <- 6
  # i <- 2

  for ( i in 1:n_week){
    temp_d <- d[, .(cut_doe, n_death, location_code)]

    # e.g. i = 1 means within one weeks after the event,
    # event is registered

    d_registered_within_i_week <- d[dor < (as.Date(cut_doe) + i*7), .(
      n_death_registered = .N,
      p_death_registered = sum(dor < (as.Date(cut_doe) + i*7))/n_death,
      n_death_total = n_death),
      keyby = .(cut_doe, location_code)]

    temp_d[,paste0("n0_", (i-1)) := 0]
    temp_d[,paste0("p0_", (i-1)) := 0]
    temp_d[d_registered_within_i_week, on= .(cut_doe, location_code),  paste0("n0_", (i-1)) := n_death_registered]
    temp_d[d_registered_within_i_week, on= .(cut_doe, location_code),  paste0("p0_", (i-1)) := p_death_registered]

    # not sure about merging back to the original super long one

    retval[[i ]] <- temp_d
    cat(paste(i, 'done \n'))
  }

  d_within_week <- cbind.data.frame(retval)
  d_within_week <- unique(as.data.table(d_within_week))
  d_within_week <- as.data.table(subset(d_within_week, select = unique(colnames(d_within_week))))





  # all dates, all locations ----
  # expand all dates and locations
  # prevent missing weeks in raw data
  first_date <- d$cut_doe[1]
  last_date <- d$cut_doe[nrow(d)]

  # dates <- unique(d$cut_doe)
  dates <- seq.Date(from = first_date,
                    to = last_date,
                    by = 7)
  locations <- unique(d$location_code)

  all_dates_locations <- expand.grid(
    cut_doe = dates,
    location_code = locations
  )

  # merge with previous step
  test <- merge(d_within_week, all_dates_locations, on = c("cut_doe, location_code"), all = TRUE)

  for(i in 0:n_week){
    test[is.na(n_death), paste0("n0_",(i)) := 0]
  }

  test[is.na(n_death), n_death := 0]
  d_within_week <- test
  d_corrected <- d_within_week[, .(cut_doe,location_code, n_death, n0_0, p0_0)]
  date_0 <- as.Date(cut(aggregation_date, "week")) # monday of aggregation
  # this is actually UNcorrected (%registered within one week)






  # NA adjustments ----

  # insert NA where we do not have data
  for ( i in 2:n_week){

    week_n <- paste0("n0_",(i-1))
    week_p <- paste0("p0_",(i-1))
    d_within_week[, new_value := NA]
    d_within_week[, temp_variable_n := get(week_n)]
    d_within_week[, temp_variable_p := get(week_p)]


    d_within_week[cut_doe >= (last_date- (i-2)*7)]#, temp_variable_n := new_value]


    d_within_week[cut_doe >= (last_date- (i-2)*7), temp_variable_n := new_value]
    d_within_week[cut_doe >= (last_date- (i-2)*7), temp_variable_p := new_value]


    d_corrected[ d_within_week,
                 on = c("cut_doe", "location_code"),
                 paste0("n0_",(i-1)) := temp_variable_n]
    d_corrected[ d_within_week,
                 on = c("cut_doe", "location_code"),
                 paste0("p0_",(i-1)) := temp_variable_p]
  }

  d_corrected[, week := isoweek_n_temp(cut_doe)]
  d_corrected[, year := isoyear_n_temp(cut_doe)]
  if(!is.null(pop_data)){
    if ("week" %in% colnames(pop_data)){
      d_corrected[pop_data, pop := pop, on = c("year", "week", "location_code")]
    }else{
      d_corrected[pop_data,
                  on = c("year", "location_code"),
                  pop := pop]
    }
  }

  ## Save rds
  # data_fake_nowcasting_county_aggregated <- d_corrected
  # save(data_fake_nowcasting_county_aggregated, file = "data/data_fake_nowcasting_county_aggregated.rda", compress = "bzip2")





  retval <- d_corrected

  class(retval) <- unique(c("nowcast_aggregate_data_v1", class(retval)))

  return (retval)
}


#' Evaluate aggregate data
#'
#' @description
#' Tries to impute missing values by performing smart assignment on all columns that are missing data.
#' E.g. if \code{location_code='norge'} then we know that \code{granularity_geo='nation'}.
#'
#' @section nowcast_aggregate_data_v1:
#' The **columns in bold** will be used to impute the listed columns.
#'
#' **location_code**:
#' - granularity_geo
#' - country_iso3
#'
#' **isoyear** (when `granularity_time=="isoyear"`):
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' **isoyearweek** (when `granularity_time=="isoweek"`):
#' - isoyear
#' - isoweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#' - date
#'
#' **date** (when `granularity_time=="day"`):
#' - isoyear
#' - isoweek
#' - isoyearweek
#' - season
#' - seasonweek
#' - calyear
#' - calmonth
#' - calyearmonth
#'
#' With regards to the time columns, `granularity_time` takes precedence over everything.
#' If `granularity_time` is missing, then we try to impute `granularity_time` by seeing if
#' there is only one time column with non-missing data. Due to the multitude of time columns,
#' `granularity_time` is an extremely important column and should always be kept with valid values.
#'
#' @param x An object of type nowcast_aggregate_data_v1 created by \code{\link{nowcast_aggregate}}
#' @param ... Not used.
#' @export
evaluate_aggregate_data <- function(x, ...) {
  UseMethod("evaluate_aggregate_data", x)
}

#' @method evaluate_aggregate_data nowcast_aggregate_data_v1
#' @export
evaluate_aggregate_data.nowcast_aggregate_data_v1 <- function(x, ...) {

  # allows us to print
  data.table::shouldPrint(x)

  return(invisible(x))
}

