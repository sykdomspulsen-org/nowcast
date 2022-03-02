# data ----
#' Aggregate weekly data for events and delay distribution, expressed by weekly cumulatives.
#'
#' @format
#' \describe{
#' \item{isoyearweek_event}{First date of every week}
#' \item{location_code}{Location code}
#' \item{monday_doe}{Date of the Monday for this isoyearweek}
#' \item{n_event}{Number of deaths this week, recorded until the aggregation date}
#' \item{n_cum_lagi}{Number of registered deaths within the next i weeks. 0 suggests the current week}
#' \item{p_cum_lagi}{Percentile of registrations within the next i weeks. 0 suggests the current week}
#' }
"d_fake_weekly_doe_dor_aggregated"



# function ----

#' DOE/DOR data aggregation
#'
#' Aggregates daily mortality reporting data to a weekly basis.
#' For more details see the help vignette:
#'
#' \code{vignette("intro", package="nowcast")}
#'
#' @param data Raw data containing doe (Date of event), dor (Date of registration or reporting) and location_code. The columns must have these exact names.
#' @param aggregation_date Date of aggregation
#' @param max_week_delay Maximum weeks of delay taken into account. Must be greater than 2.
#' @param keep_weekly_prob Binary. If True, the final aggregated data has weekly reporting percentile.
#' @examples
#'
#' data <- nowcast::data_fake_nowcasting_raw
#' aggregation_date <- as.Date("2020-01-01")
#' max_week_delay <- 4
#'
#' weekly_counts <- count_weekly_reporting(data, aggregation_date, max_week_delay)
#'
#' weekly_counts_p <- count_weekly_reporting(data, aggregation_date, max_week_delay, keep_weekly_prob = TRUE)
#' @return Aggregated weekly data with number of events happened each week and reporting delay distribution
#'
#'
#' @export



count_weekly_reporting <- function(
  data,
  aggregation_date,
  max_week_delay,
  keep_weekly_prob = F) {


  doe <- NULL
  dor <- NULL
  location_code <- NULL
  monday_doe <- NULL
  monday_dor <- NULL
  isoyearweek_event <- NULL
  lag_register <- NULL


  ##### for developing
  # data <- nowcast::data_fake_nowcasting_raw
  # aggregation_date <- as.Date("2019-12-31")
  # max_week_delay <- 4
  # keep_weekly_prob <- T

  # check colnames ---- #

  if (! "doe" %in% colnames(data)){
    stop("The dataset does not have the correct column names, doe is missing")
  }
  if (! "dor" %in% colnames(data)){
    stop("The dataset does not have the correct column names, dor is missing")
  }
  if (! "location_code" %in% colnames(data)){
    stop("The dataset does not have the correct column names, location_code is missing")
  }

  if(max_week_delay <2){
    stop("Number of reporting delay weeks need to be greater or equal to 2 weeks")
  }

  d <- data.table(data)
  d <- d[, .(doe, dor, location_code)]


  # cut(aggregation_date + 1:7, 'day')
  # cut(aggregation_date + 1:7, 'week') # 2 levels (weeks)
  # fhidata::days[mon == '2020-01-06']

  # only keep part before aggregation date
  # convert aggre_date into monday of that week
  # e.g. 2020-12-31 is a tuesday, so it'll be converted to 12-30 (monday)


  # code as week ----
  d <- d[dor < as.Date(cut(aggregation_date, "week"))]
  d <- d[doe < as.Date(cut(aggregation_date, "week"))]

  d <- d[order(doe, dor)]
  d

  dcopy <- copy(d)
  # cut_doe is the monday of that week
  # fhidata::days[mon == '2018-01-01']
  # fhidata::days[mon == '2019-12-23']


  dcopy[, monday_doe := as.Date(cut(doe, "week"))]
  dcopy[, monday_dor := as.Date(cut(dor, "week"))]


  dcopy[, isoyearweek_event := spltime::date_to_isoyearweek_c(monday_doe)]


  # registration lag in weeks
  dcopy[, lag_register := as.numeric((monday_dor - monday_doe)/7)]

  # dcopy[monday_dor - monday_doe == 0, lag_register := 0]
  # dcopy[monday_dor - monday_doe == 7, lag_register := 1]
  # dcopy[monday_dor - monday_doe == 14, lag_register := 2]
  # in this dat only 2 weeks lag, but in reality this can be 6 weeks


  # count events and register ----

  # all deaths ever happend (the last several weeks can be low due to delay)
  d_event_total <- dcopy[
    , .("n_event" = .N), keyby = .(
      isoyearweek_event,
      location_code
    )]

  # deaths registered within each week
  d_register_lags <- vector("list", length = max_week_delay+1)

  for(i in 1:(max_week_delay+1)){
    # i <- 1
    d_register_lags[[i]] <- dcopy[
      lag_register <= (i-1), .("n_cum" = .N), keyby = .(
        isoyearweek_event,
        location_code
      )]
    setnames(d_register_lags[[i]], 'n_cum', paste0('n_cum_lag', i-1))
  }


  # merge total event and registered each week into one
  list_event_reg <- c(list(d_event_total), d_register_lags)
  list_event_reg
  lapply(list_event_reg, function(i) setkey(i, 'isoyearweek_event', 'location_code'))

  d_event_reg <- Reduce(function(...) merge(..., all = T), list_event_reg)
  d_event_reg


  # if need to compute weekly reporting probability
  if(keep_weekly_prob == T){
    n_cum_cols <- paste0('n_cum_lag', 0:max_week_delay)
    p_cum_cols <- paste0('p_cum_lag', 0:max_week_delay)

    d_event_reg[, (p_cum_cols):= lapply(.SD, function(x){x/get('n_event')}),
                .SDcols = n_cum_cols]

  }


  # expand, fill in missing ----
  # expand for all location_code, all dates

  monday_doe_min <- min(dcopy$monday_doe)
  monday_doe_max <- max(dcopy$monday_doe)

  monday_doe_all <- seq.Date(from = monday_doe_min,
                             to = monday_doe_max,
                             by = 7)

  locations <- unique(d$location_code)
  # locations <- c('norge', 'county03')
  all_dates_locations <- expand.grid(
    monday_doe = monday_doe_all,
    location_code = locations
  )
  setDT(all_dates_locations)
  all_dates_locations[, isoyearweek_event := spltime::date_to_isoyearweek_c(monday_doe)]

  # combine these two
  d_aggre <- merge(all_dates_locations, d_event_reg,
        by = c('isoyearweek_event', 'location_code'),
        all = T)


  # need to name it as exactly the same name
  # d_fake_weekly_doe_dor_aggregated <- d_aggre
  # save(d_fake_weekly_doe_dor_aggregated, file = "data/d_fake_weekly_doe_dor_aggregated.rda", compress = "bzip2")

  retval <- d_aggre
  return (retval)
}





