#' Join SVG plots. This function first populates each row in the first column, then rows in the seccond column
#'
#' @param ... mumltiple character vectors with SVG content
#' @param nrows number of rows of plots in joint plot, default is set to number of plots
#' @param ncols number of columns of plots in joint plot, default is set to 1
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
join_plots <- function(..., nrows = length(list(...)), ncols = 1){

  n_plots <-length(list(...))

  # check if there are enough rows and cols to show all plots
  stopifnot(n_plots <= nrows * ncols)

  plots <- array(data = list(...), dim = c(nrows, ncols))
  widths <- matrix(apply(plots, c(1,2), function(svg_string) get_svg_size(svg_string)[1]), nrow = nrows, ncol = ncols)
  heights <- matrix(apply(plots, c(1,2), function(svg_string) get_svg_size(svg_string)[2]), nrow = nrows, ncol = ncols)
  cumulated_widths <- matrix(apply(widths, 1, cumsum), nrow = nrows, ncol = ncols, byrow = T)
  cumulated_heights <- matrix(apply(heights, 2, cumsum), nrow = nrows, ncol = ncols)

  joint_height <- max(colSums(heights))

  joint_width <- max(rowSums(widths))

  result_string <- initialize(height = joint_height, width = joint_width)

  for(j in 1:ncols){
    for(i in 1:nrows){
      if ((j-1) * nrows  + i > n_plots) {
        break()
      }
      translate_x <- cumulated_widths[i,j] - widths[i,j]
      translate_y <- cumulated_heights[i,j] - heights[i,j]
      result_string <- paste(
        result_string,
        translate_svg(plots[i,j],
                      translate_x, translate_y)
      )
    }
  }
  result_string <- finalize(result_string)
  return(result_string)
}


translate_svg<- function(svg_string, x, y){
  return(paste('<g transform="translate(',
        x,
        ' ',
        y,
        ')">',
        svg_string,
        '</g>'))
}


facet_chart <- function(data, facet_by, chart_type, ...){
  categories <- unique(data[[facet_by]])
  for(category in categories){
    filtered <- data[data[facet_by] == category]
    print(category)
    print(filtered)
  }
}
