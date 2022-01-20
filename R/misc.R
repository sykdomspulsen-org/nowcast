#' isoyearweek_to_year_n
#' isoyearweek to year (numeric)
#' This function breaks the string connected with '-' into year/week
#' @param yrwk Year-week, e.g. "2020-19" for 19th week in 2020
#' @export
isoyearweek_to_year_n <- function(yrwk){
  year_n <- stringr::str_split(yrwk, pattern = '-') %>%
    purrr::map_chr(., function(x){x[1]}) %>% as.numeric()
  return(year_n)
}

