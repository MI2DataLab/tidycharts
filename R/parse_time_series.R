parse_time_series <- function(df, x, y){
  x <- df[[x]]
  result.y <- df[[y]]
  mon.lengths <- lubridate::days_in_month(x)
  result.months <- names(mon.lengths)
  result.x <- lubridate::day(x) / mon.lengths * 100
  result <- data.frame(x = result.x,
                       y = result.y,
                       mon = result.months)
  row.names(result) <- NULL
  return(result)
}
