# no more than n data series

stop_if_many_series <- function(series, max_series){
  stopifnot(length(series) <= max_series)
}
# no more than x time periods
stop_if_many_categories <- function(x, max_categories){
  stopifnot(length(x) <= max_categories)
}

# both positive and negative values to visualize
stop_if_pos_neg_values <- function(df, series){
  bool_vec <- apply(df[series], 1, function(x) all(-x >= 0) | all(x >= 0))
  stopifnot(all(bool_vec))
}

stop_if_variance_colors <- function(colors_encoding){
  stopifnot(colors_encoding %in% c(1,2))
}
