
#---
add_bar_basic <-
  function(shift,
           data,
           cat,
           series,
           k ,
           y,
           width_of_one,
           series_labels = NULL,
           df_with_real_values = NULL,
           style = NULL) {
    #k-który to wiersz
    #df_with_real_values - to show real values after the normalization

  all_sums <- rowSums(data[series])
  svg_string <- ""
  labels <- ""
  x <- 80 + shift
  for (i in 1:length(series)){ #going through series
    value <- data[,series[i]] #a vector
    color <- get_gray_color_stacked(i)

    if(is.null(series_labels)==FALSE){
      if(length(series)>1){
      labels <- paste(labels,
                      #series label
                      add_label((x+(width_of_one*(value[k])/2)), y+12-16, series_labels[i]),
                      sep='\n')}
    }

    if(value[k] < 0){
      x <- x - (width_of_one*abs(value[k]))}

    rect <- draw_rect(x, y, color$bar_color, (width_of_one*abs(value[k])), 16, style = style)

    if(is.null(df_with_real_values)==FALSE){value_text <- df_with_real_values[,series[i]][k]}
    else{value_text <- value[k]}
    #checking if there's enough place for a label
    if(str_width(abs(value_text))+3.2 < abs(value_text)*width_of_one && length(series) > 1){

      labels <- paste(
        labels,
        #each series value label
        add_label((x+(width_of_one*(abs(value[k]))/2)), y+12, value_text, color=color$text_color),
        sep='\n'
      )
    }
    svg_string <- paste(svg_string, rect, labels,  sep = '\n')
    if(value[k] > 0){ x <- x + (width_of_one*value[k])}

  }
  if(value[k]>0){sum_label <- add_label((x+4.8), y+12,all_sums[k], anchor="start")}
  else{sum_label <- add_label(80+shift+4.8, y+12, all_sums[k], anchor="start")}
  return(paste(svg_string,
               # value label
               #add_label((x+4.8), y+12,all_sums[k], anchor="start"),
               sum_label,
               #category label
               add_label( 72.2, y+14, cat[k], anchor="end"),
               #vertical axis
               draw_line(80+shift, 80+shift,(y-4.8), (y+16+4.8)),
               labels,
               sep = '\n'
  ))
}

#---
draw_bars_basic <- function(svg_string, data, cat, series, series_labels, df_with_real_values=NULL, styles = NULL, shift = 0){
  bars <- svg_string
  y <- 50
  all_sums <- rowSums(data[series])
  width_of_one <- 200/max(abs(all_sums))



  if (length(series) > 1) {
    styles <-  NULL # delete styles if there is more than one series
  }

  bars <- paste(bars,
                add_bar_basic(shift, data,cat, series,1, y, width_of_one, series_labels, df_with_real_values, style  = styles[1]),
                sep='\n')
  y <- y+24
  for(i in 2:length(cat)){
    bars <- paste(bars,
                  add_bar_basic(shift ,data,cat, series,i, y, width_of_one, df_with_real_values=df_with_real_values, style = styles[i]),
                  sep='\n')
    y <- y+24
  }
  return (bars)
}



#---
#' Generates basic horizontal barchart. If more than one series is supplied, stacked barchart is generated.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector cointaining category names of values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param styles optional vector with styles of bars
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"

barchart_plot <- function(data, cat, series, series_labels, styles = NULL){
  # TODO all values in one bar should have the same sign
  initialize() %>%
    draw_bars_basic(.,data, cat, series, series_labels, styles = styles) %>%
    finalize()
}

#---

#' Generates basic horizontal barchart with index on a given value. If more than one series is supplied, stacked barchart is generated.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector cointaining category names of values
#' @param series vector containing names of columns in data with values to plot
#' @param index_val numeric value of the index
#' @param series_labels vector containing names of series to be shown on the plot
#' @param styles optional vector with styles of bars
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples

barchart_plot_index <- function(data, cat, series, index_val, series_labels, styles = NULL){

  all_sums <- rowSums(data[series])
  width_of_one <- 200/max(all_sums)

  #dealing with negative values
  neg <- all_sums[all_sums < 0]
  shift <- width_of_one*abs(min(neg))
  if(is.finite(shift)==FALSE){shift <- 0} #in case there are no negative values

  initialize() %>%
    paste(.,
          draw_bars_basic("",data, cat, series, series_labels, styles = styles, shift = shift),
          add_vertical_index(80+(width_of_one*index_val)+shift, (66+24*(length(cat)-1))),
          sep='\n') %>%
    finalize()
}

#---
#'  Generates normalized horizontal barchart. If more than one series is supplied, stacked barchart is generated.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector cointaining category names of values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
barchart_plot_normalized <- function(data, cat, series, series_labels){
  df <- normalize_rows(data, cat, series)
  y_end <- 50 + 24*length(cat)
  # TODO all values should have the same sign
  initialize() %>%
    #draw_bars_normalized(.,data, cat, series, series_labels) %>%
    draw_bars_basic(.,df, cat, series, series_labels, data) %>%
    paste(.,
          add_vertical_index(280, (y_end+16+4.8-24)),
          draw_rect(285, 50, "white", 25, y_end, style = "total_white"), #it covers the sum labels
          sep='\n') %>%
    finalize()
}
