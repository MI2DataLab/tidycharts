
pkg.env <- new.env(parent = emptyenv())

pkg.env$styles_df <-
  rbind(
    actual = c("rgb(64,64,64)", "rgb(64,64,64)"),
    previous =
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

#colors for scatter plots
pkg.env$scatter_colors <-c(
    "rgb(61, 56, 124)",
    "rgb(0, 200, 154)",
    "rgb(113, 103, 177)",
    "rgb(0, 150, 193)" ,
    "rgb(249, 248, 113)",
    "rgb(147, 67, 134)"
  )
get_scatter_colors <- function(series_number, scatter_colors = pkg.env$scatter_colors){

  stopifnot(series_number %in% 1:6)
  #return(list(bar_color = colors_df[series_number,][['bar_colors']],
  #            text_color = colors_df[series_number,][['text_colors']]))
  return(scatter_colors[series_number])
}



#' Function to get bar/area color for stacked plots.
#'
#' @param series_number what is the number of the series. one of 1:6.
#' @param colors_df data frame with variety of colors
#'
#' @return list with bar_color and text_color
#'
get_gray_color_stacked <- function(series_number, colors_df = pkg.env$colors_df){

  stopifnot(series_number %in% 1:6)
  return(list(bar_color = colors_df[series_number,][['bar_colors']],
              text_color = colors_df[series_number,][['text_colors']]))
}

get_interval_width <- function(interval){
  stopifnot(interval %in% c("days", "weeks", "months", "quarters", "years"))
  return(list(
    bar_width = pkg.env$widths[[interval, "bar_width"]],
    category_width = pkg.env$widths[[interval, "category_width"]]
  ))
}



#' Change default colors of the package
#'
#' Customize your plots and change default color palette.
#'
#' @param colors_df data frame with 6 rows and 2 columns. Columns must nave names : "text_colors", "bar_colors". In cells there should be rgb values of chosen colors in format: "rgb(x,y,z)". Rows represent subsequent colors on stacked plots.
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' mi2lab_colors <- cbind(
#'   bar_colors =  c(
#'    "rgb(68, 19, 71)",
#'    "rgb(243, 46, 255)",
#'    "rgb(106, 0, 112)",
#'    "rgb(217, 43, 227)" ,
#'    "rgb(114, 49, 117)",
#'    "rgb(249, 110, 255)"
#'  ),
#' text_colors = c("white", "white", "white", "white", "white", "white"))
#'
#' set_colors(mi2lab_colors)
#'
set_colors <- function(colors_df){
  stopifnot(all(dim(colors_df) == c(6,2)))
  stopifnot(all(dimnames(colors_df)[[2]] %in% c("text_colors", "bar_colors")))
  pkg.env$colors_df <- colors_df
}




#' Change default styles for plots
#'
#' @param styles_df data frame with columns 'fill' and 'stroke'. Rows represent subsequent styles which names can be passed to plotting functions, usually as styles argument.
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' styles_df <-
#'   rbind(
#'   actual = c("rgb(64,64,64)", "rgb(64,64,64)"),
#'   previous =
#'     c("rgb(166,166,166)", "rgb(166,166,166)"),
#'   forecast =
#'     c("url(#diagonalHatch)", "rgb(64,64,64)"),
#'   plan = c("white", "rgb(64,64,64)"),
#'   total_white = c("white", "white")
#'   )
#' colnames(styles_df) <- c("fill", "stroke")
#'
#' set_styles(styles_df)
set_styles <- function(styles_df){
  stopifnot(colnames(styles_df) %in% c('stroke', 'fill'))
  pkg.env$styles_df <-styles_df
}

#' Restore default color and style settings
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#'
#' restore_defaults()
restore_defaults <- function() {
  pkg.env$styles_df <-
    rbind(
      actual = c("rgb(64,64,64)", "rgb(64,64,64)"),
      previous =
        c("rgb(166,166,166)", "rgb(166,166,166)"),
      forecast =
        c("url(#diagonalHatch)", "rgb(64,64,64)"),
      plan = c("white", "rgb(64,64,64)"),
      total_white = c("white", "white")
    )
  colnames(pkg.env$styles_df) <- c("fill", "stroke")

  pkg.env$colors_df <- cbind(
    bar_colors =  c(
      "rgb(64,64,64)",
      "rgb(166,166,166)",
      "rgb(70,70,70)",
      "rgb(90,90,90)" ,
      "rgb(110,110,110)",
      "rgb(127,127,127)"
    ),
    text_colors = c("white", "black", "white", "white", "white", "black"))
}
