
pkg.env <- new.env(parent = emptyenv())

pkg.env$styles_df <-
  rbind(
    actual = c("rgb(64,64,64)", "rgb(64,64,64)"),
    prevoius =
      c("rgb(166,166,166)", "rgb(166,166,166)"),
    forecast =
      c("url(#diagonalHatch)", "rgb(64,64,64)"),
    plan = c("white", "rgb(64,64,64)"),
    total_white = c("white", "white")
  )
colnames(pkg.env$styles_df) <- c("fill", "stroke")

pkg.env$widths <- data.frame(
  interval = c('days', 'weeks', 'months', 'quarters', 'years'),
  bar_width = c(16, 21.33, 32, 37.33, 42.66),
  category_width = c(24, 32, 48, 56, 64)
)

rownames(pkg.env$widths) <- pkg.env$widths$interval

pkg.env$colors_df <- cbind(
  bar_colors =  c(
    "rgb(64,64,64)",
    "rgb(166,166,166)",
    "rgb(70,70,70)",
    "rgb(90,90,90)" ,
    "rgb(110,110,110)",
    "rgb(127,127,127)"
  ),
  text_colors = c("white", "black", "white", "white", "white", "black")
)

get_style <- function(style, styles_df = pkg.env$styles_df){
  return(styles_df[style, ])
}

#' Function to get bar/area color for stacked plots.
#'
#' @param series_number what is the number of the series. one of 1:6.
#' @param colors_df data frame with variety of colors
#'
#' @return list with bar_color and text_color
#'
#' @examples
get_gray_color_stacked <- function(series_number, colors_df = pkg.env$colors_df){

  stopifnot(series_number %in% 1:6)
  return(list(bar_color = colors_df[series_number,][['bar_colors']],
              text_color = colors_df[series_number,][['text_colors']]))
}
