#' Join SVG charts This function first populates each place in the first row, then columns in the second row
#'
#' @param ... mumltiple character vectors with SVG content
#' @param nrows number of rows of plots in joint plot, default is set to number of plots
#' @param ncols number of columns of plots in joint plot, default is set to 1
#' @param list_of_plots optional list of plots to join. Use exclusively ... params or list_of_plots. Names of list entries will be plotted as titles of the plots
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
#' join_charts(
#'   column_chart(df, x = 'mon', series = 'values'),
#'   column_chart(df, x = 'mon', series = 'values')
#' ) %>% SVGrenderer()
join_charts <- function(..., nrows = max(length(list(...)), length(list_of_plots)),
                        ncols = 1, list_of_plots = NULL){

  if (!is.null(list_of_plots)) {
    plots <- list_of_plots
    titles <- names(list_of_plots)
  }else{
    plots <- list(...)
    titles <- rep('', length(plots))
  }
  n_plots <-length(plots)

  # check if there are enough rows and cols to show all plots
  stopifnot(n_plots <= nrows * ncols)
  plots <- tryCatch( # if we pass not full matrix(5 elements to 3x2 matrix) we get warning
                     # so we use tryCatch to ensure no warnings are displayed
    matrix(data = plots, nrow = nrows, ncol = ncols, byrow = T),
    error = function(cond){
      for(i in (n_plots+1):nrows * ncols) plots[[i]] <- '<svg height="0" width="0"></svg>'
      matrix(data = plots, nrow = nrows, ncol = ncols, byrow = T)
    },
    warning = function(cond){
      for(i in (n_plots+1):(nrows * ncols)) plots[[i]] <- '<svg height="0" width="0"></svg>'
      matrix(data = plots, nrow = nrows, ncol = ncols, byrow = T)
    }
   )

  widths <- matrix(apply(plots, c(1,2), function(svg_string) get_svg_size(svg_string)[1]), nrow = nrows, ncol = ncols)
  heights <- matrix(apply(plots, c(1,2), function(svg_string) get_svg_size(svg_string)[2]), nrow = nrows, ncol = ncols)
  cumulated_widths <- matrix(apply(widths, 1, cumsum), nrow = nrows, ncol = ncols, byrow = T)
  cumulated_heights <- matrix(apply(heights, 2, cumsum), nrow = nrows, ncol = ncols)

  joint_height <- max(colSums(heights))

  joint_width <- max(rowSums(widths))

  result_string <- initialize(height = joint_height, width = joint_width)

  for(i in 1:nrows){
    for(j in 1:ncols){
      if ((i-1) * ncols  + j > n_plots) {
        break()
      }
      translate_x <- cumulated_widths[i,j] - widths[i,j]
      translate_y <- cumulated_heights[i,j] - heights[i,j]
      result_string <- paste(
        result_string,
        translate_svg(plots[i,j] %>% add_title( titles[(i-1) * ncols  + j], '', '', ''),
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

#' Facet chart
#'
#' Create multiple charts with data split into groups
#'
#' @param facet_by a name of column in data, that the charts will be splitted by
#' @param ncols number of columns of the plots. Number of rows will be adjusted accordingly
#' @param FUN function to plot the basic chart
#' @param ... other parameters passed to FUN
#'
#' @inheritParams column_chart
#' @inherit join_charts return
#' @export
#'
#' @examples
#' facet_chart(
#'   data = mtcars,
#'   facet_by = 'cyl',
#'   ncols = 2,
#'   FUN = scatter_plot,
#'   x = mtcars$hp,
#'   y = mtcars$qsec,
#'   legend_title = ''
#'  ) %>% SVGrenderer()
facet_chart <- function(data, facet_by, ncols = 3, FUN, ...){

  stopifnot(facet_by %in% colnames(data))
  stopifnot(is.double(ncols), ncols > 0)
  # find if any of ... is a vector passed to FUN, like vector x passed to scatterplot
  args <- list(...)
  data_len <- dim(data)[1] # no of rows in data

  vector_args <- lapply(args, function(arg) if(length(arg) == data_len) arg)
  single_args <- lapply(args, function(arg) if(length(arg) != data_len) arg)

  vector_args <- vector_args[-which(sapply(vector_args, is.null))]
  single_args <- single_args[-which(sapply(single_args, is.null))]

  categories <- sort(unique(data[[facet_by]]))
  # get a list of subsets of data with different facet_by values
  list_data <- lapply(categories, function(category) {
    l <- list(data[data[facet_by] == category,])
    names(l) <- 'data'
    l
  })
  # filter vectoried args
  list_args <- list()
  for(category in categories){
    list_args <- append(list_args,
                        list(lapply(vector_args,
                               function(arg) arg[data[facet_by] == category])))
  }
  tmp <- mapply(c, list_data, list_args, SIMPLIFY = F)
  plots <- list()
  # draw plots of each subset of data
  for(entry in tmp){
    plots <- append(plots, do.call(FUN, c(entry, single_args)))
  }
  # add names to plots so they can be plotted with titles
  names(plots) <- paste0(facet_by,' = ', categories)
  nrows <- ceiling(length(categories) / ncols)
  join_charts(list_of_plots = plots, nrows = nrows, ncols = ncols)
}
