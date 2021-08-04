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
  initialize() %>%
    paste(...) %>%
    finalize()
}
