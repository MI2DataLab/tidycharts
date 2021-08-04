#' Join SVG plots.
#'
#' @param ... mumltiple character vectors with SVG content
#' @param nrows number of rows of plots in joint plot, default is set to number of plots
#' @param nclos number of columns of plots in joint plot, default is set to 1
#'
#' @return Character vector with SVG content
#' @export
#'
#' @examples
#'   df <- data.frame(
#'    mon = month.abb[1:6],
#'    values = rnorm(6)
#'   )
#'
#' join_plots(
#'   column_chart(df, x = 'mon', series = 'values'),
#'   column_chart(df, x = 'mon', series = 'values')
#' ) %>% SVGrenderer()
join_plots <- function(..., nrows = length(...), nclos = 1){
  plots <- list(...)
  widths <- sapply(plots, function(svg_string) get_svg_size(svg_string)[1])
  heights <- sapply(plots, function(svg_string) get_svg_size(svg_string)[2])

  result_string <- initialize(height = sum(heights), width = max(widths))
  for(i in 1:length(plots)){
    result_string <- paste(
      result_string,
      transpose_svg(plots[i],
                    0, cumsum(heights)[i] - heights[i])
    )
  }
  result_string <- finalize(result_string)
  return(result_string)
}

transpose_svg  <- function(svg_string, x, y){
  return(paste('<g transform="translate(',
        x,
        ' ',
        y,
        ')">',
        svg_string,
        '</g>'))
}
