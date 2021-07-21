#' @export
parse_time_series_column <- function(y, df, x, convert.to = 'months') {
  stopifnot(convert.to %in% c('weeks', 'months', 'quarters', 'years'))
  x <- df[[x]]
  result.y <- df[[y]]

  if (convert.to == 'weeks') {
    week.lengths <- 7
    result.categories <- lubridate::epiweek(x)
    result.x <- lubridate::wday(x) / week.lengths * 100

  } else if (convert.to == 'months') {
    mon.lengths <-lubridate::days_in_month(x)
    result.categories <- names(mon.lengths)
    result.x <- lubridate::day(x) / mon.lengths * 100

  } else if (convert.to == 'quarters') {
    quarter.lengths <- 92 # TODO get exact quarters length
    result.categories <-quarter_abbr(x)
    result.x <- lubridate::qday(x) / quarter.lengths * 100
  } else if (convert.to == 'years') {
    year.lengths <- ifelse(lubridate::leap_year(x), 366, 365)
    result.categories <- lubridate::year(x)
    result.x <- lubridate::yday(x) / year.lengths * 100
  }

  result <- data.frame(x = result.x,
                       y = result.y,
                       cat = result.categories)
  row.names(result) <- NULL
  return(result)
}

quarter_abbr <- function(x){
  paste0('Q', lubridate::quarter(x))
}

#' @export
parse_time_series <- function(df, dates, series, convert.to = 'months'){
  stopifnot(convert.to %in% c('weeks', 'months', 'quarters', 'years'))
  lapply(series, parse_time_series_column, df = df, x = dates, convert.to = convert.to)
}
