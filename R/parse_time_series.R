
parse_time_series_column <- function(y, df, x, convert.to = 'months') {
  stopifnot(convert.to %in% c('weeks', 'months', 'quarters', 'years'))
  x <- get_vector(df, x)
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

#' Function to transfer data frame with time series values in wide format to format accepted by `line_chart_dense_custom`.
#'
#' @param df Date frame with data in wide format.
#' @param dates Name of column in `df` which contains dates.
#' @param series Vector of column names in `df` with values of time series.
#' @param convert.to Granularity of x axis. One of c('weeks', 'months', 'quarters', 'years'). Default value is 'months'.
#'
#' @return list of data frames, each one containing data about one time series.  Data frames in returned list consist of columns:
#' * containing numeric values from 0 to 100 defining the percentage of distance in one time interval of the point (x - coordinattes of the point)
#' * containing the value of a point  (y - coordinates of the point)
#' * containing the time interval of the value
#'
#' @export
#' @examples
#' df <- data.frame(
#'     dates = as.Date(c('2021-07-12', '2021-06-18', '2021-05-12')),
#'     val1 = c(1.5, 1.2, 2.1),
#'     val2 = c(0.9, 3.2, 1.1))
#' parse_time_series(df, 'dates', c('val1', 'val2'))
parse_time_series <- function(df, dates, series, convert.to = 'months'){
  stopifnot(convert.to %in% c('weeks', 'months', 'quarters', 'years'))
  lapply(series, parse_time_series_column, df = df, x = dates, convert.to = convert.to)
}
