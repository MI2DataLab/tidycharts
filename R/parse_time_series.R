parse_time_series <- function(df, x, y, convert.to = 'months') {
  stopifnot(convert.to %in% c('weeks', 'months', 'quarters', 'years'))
  x <- df[[x]]
  result.y <- df[[y]]

  if (convert.to == 'weeks') {
    week.lengths <- 7
    result.categories <- paste(lubridate::epiweek(x), lubridate::year(x), sep = "\n")
    result.x <- lubridate::wday(x) / week.lengths * 100

  } else if (convert.to == 'months') {
    mon.lengths <-lubridate::days_in_month(x)
    result.categories <-  paste(names(mon.lengths), lubridate::year(x), sep = "\n")
    result.x <- lubridate::day(x) / mon.lengths * 100

  } else if (convert.to == 'quarters') {
    quarter.lengths <- 92 # TODO get exact quarters length
    result.categories <- paste(quarter_abbr(x), lubridate::year(x), sep = "\n")
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
