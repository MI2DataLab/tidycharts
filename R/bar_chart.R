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
           style = NULL,
           color = NULL,
           show_series_labels = length(series) > 1,
           ax_style = NULL,
           neg_pos = 'right') {
    #k-który to wiersz
    #df_with_real_values - to show real values after the normalization

    all_sums <- rowSums(data[series])
    svg_string <- ""
    labels <- ""
    x <- get_margins()$left + shift
    for (i in 1:length(series)){ #going through series
      value <- data[,series[i]] #a vector
      if (is.null(color)) curr_color <- get_color_stacked(i)
      else curr_color <- color
      if(is.null(series_labels)==FALSE){
        if(show_series_labels){
          labels <- paste(labels,
                          #series label
                          add_label((x+(width_of_one*(value[k])/2)), y+8-16, series_labels[i]),
                          sep='\n')}
      }

      if(value[k] < 0){
        x <- x - (width_of_one*abs(value[k]))}

      rect <- draw_rect(x, y, curr_color$bar_color, (width_of_one*abs(value[k])), 16, style = style)

      if(is.null(df_with_real_values)==FALSE){value_text <- round(df_with_real_values[,series[i]][k],1)}
      else{value_text <- value[k]}
      #checking if there's enough place for a label
      if(str_width(abs(value_text)) + 3.2 < abs(value[k])*width_of_one && length(series) > 1){

        labels <- paste(
          labels,
          #each series value label
          add_label((x+(width_of_one*(abs(value[k]))/2)), y+12, value_text, color=curr_color$text_color),
          sep='\n'
        )
      }
      svg_string <- paste(svg_string, rect,  sep = '\n')
      if(value[k] > 0){ x <- x + (width_of_one*value[k])}

    }
    if(value[k] >= 0) {
      sum_label <- add_label((x + 4.8), y + 12, all_sums[k], anchor = "start")
    }
    else{
      if (neg_pos == 'right') {
        sum_label <-
          add_label(get_margins()$left + shift + 4.8, y + 12, all_sums[k], anchor = "start")
      } else{
        sum_label <-
          add_label((x - 4.8), y + 12, all_sums[k], anchor = "end")
      }
    }

    return(paste(svg_string,
                 # value label
                 #add_label((x+4.8), y+12,all_sums[k], anchor="start"),
                 sum_label,
                 #category label
                 add_label(get_margins()$left - 7.8, y+14, cat[k], anchor="end"),
                 #vertical axis
                 ifelse(is.null(ax_style),
                        draw_line(get_margins()$left+shift, get_margins()$left+shift,(y-4.8), (y+16+4.8)),
                        draw_rect(get_margins()$left+shift, (y-4.8), color = NULL, width = 4.8, height = 16 + 9.6, style = ax_style)
                 ),
                 labels,
                 sep = '\n'
    ))
  }


add_pin <-function(shift,
                   value,
                   cat,
                   y,
                   width_of_one,
                   series_labels = '',
                   style = NULL,
                   color = NULL,
                   ax_style = NULL,
                   show_series_labels = T) {
  x <- get_margins()$left + shift

  if(value < 0){
    label_offset <- -10
    label_anchor <- 'end'
  }else{
    label_offset <- 10
    label_anchor <- 'start'
  }

  svg_strng <- ''
  # draw axis
  svg_string <- draw_rect(get_margins()$left+shift - 2.4, (y - 12), color = NULL, width = 4.8, height = 16 + 8, style = ax_style)
  # draw category label
  svg_string <- paste(svg_string, add_label(get_margins()$left - 7.8, y + 6, cat, anchor="end"), sep = '\n')
  # draw pin head
  svg_string <- paste(svg_string,
                      draw_rect(get_margins()$left+shift + width_of_one * value - 5.6, y - 5.6 , color = NULL, width = 11.2, height = 11.2),
                      sep = '\n')
  # draw pin line
  svg_string <- paste(svg_string,
                      draw_rect(get_margins()$left+shift, y - 2.4 , color = color$bar_color, width = width_of_one * value, height = 4.8),
                      sep = '\n')
  # draw pin label
  svg_string <- paste(svg_string,
                      add_label(get_margins()$left+shift + width_of_one * value + label_offset, y + 6, format(value, digits = 2), anchor = label_anchor),
                      sep = '\n')
  return(svg_string)
}

#---
draw_bars_basic <- function(svg_string, data, cat, series, series_labels, df_with_real_values=NULL, styles = NULL, shift = 0){
  bars <- svg_string
  y <- get_margins()$top
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
#' @param cat vector containing category names of values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param styles optional vector with styles of bars
#'
#' @return object of class tidychart with a character vector containing SVG elements
#' @export
#'
#' @examples
#' #prepare the data frame
#' data <- data.frame(
#' city = c("Berlin", "Munich", "Cologne", "London", "Vienna", "Paris", "Zurich"),
#' Products = c(538, 250, 75, 301, 227, 100, 40),
#' Services = c(621, 545, 302, 44, 39, 20, 34)
#')
#' #generate svgstring
#' barchart <- bar_chart(data, data$city, c("Products", "Services"), c("Products", "Services"))
#'
#' #show the plot
#' barchart
#'
#'
#' @importFrom magrittr "%>%"
bar_chart <- function(data, cat, series, series_labels = series, styles = NULL){

  all_sums <- rowSums(data[series])
  width_of_one <- 200/max(abs(all_sums))
  if(length(cat) == 1){
    cat <- data[,cat]
  }
  #dealing with negative values
  neg <- all_sums[all_sums < 0]
  if(length(neg) == 0){shift <- 0}
  else{shift <- width_of_one*abs(min(neg))}

  svg_string <- initialize(y_vector = cat,
             bar_width = 16) %>%
    draw_bars_basic(data, cat, series, series_labels, styles = styles, shift = shift) %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}

#---

#' Generates basic horizontal barchart with index on a given value. If more than one series is supplied, stacked barchart is generated.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector containing category names of values
#' @param series vector containing names of columns in data with values to plot
#' @param ref_val numeric value of the index
#' @param series_labels vector containing names of series to be shown on the plot
#' @param styles optional vector with styles of bars
#' @param ref_label string defining a text that should be displayed in the referencing line. Set by default to index_val.
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#'
#' #prepare the data frame
#' data <- data.frame(
#' city = c("Berlin", "Munich", "Cologne", "London", "Vienna", "Paris", "Zurich"),
#' Products = c(538, 250, 75, 301, 227, 100, 40),
#' Services = c(621, 545, 302, 44, 39, 20, 34)
#')
#' #create svg string
#' barchart_ref <- bar_chart_reference(data, data$city, c("Products"), 100, c("Products"))
#'
#' #show the plot
#' barchart_ref
#'
bar_chart_reference <- function(data, cat, series, ref_val, series_labels = series, styles = NULL, ref_label=ref_val){

  all_sums <- rowSums(data[series])
  width_of_one <- 200/max(abs(all_sums))
  if(length(cat) == 1){
    cat <- data[,cat]
  }

  #dealing with negative values
  neg <- all_sums[all_sums < 0]
  if(length(neg) == 0){shift <- 0}
  else{shift <- width_of_one*abs(min(neg))}

  svg_string <-initialize(width= get_margins()$left + shift + 250, height = get_margins()$top + 24*length(cat)) %>%
    paste(draw_bars_basic("",data, cat, series, series_labels, styles = styles, shift = shift),
          add_vertical_index(get_margins()$left+(width_of_one*ref_val)+shift, (get_margins()$top + 16 + 24*(length(cat)-1)), ref_label),
          sep='\n') %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}

#---
#'  Generates normalized horizontal barchart. If more than one series is supplied, stacked barchart is generated.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector containing category names of values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' #prepare the data frame
#' data <- data.frame(
#' city = c("Berlin", "Munich", "Cologne", "London", "Vienna", "Paris", "Zurich"),
#' Products = c(538, 250, 75, 301, 227, 100, 40),
#' Services = c(621, 545, 302, 44, 39, 20, 34)
#')
#' #create svg string
#' barchart_normalized <- bar_chart_normalized(
#'   data = data,
#'   cat = data$city,
#'   series = c("Products", "Services"))
#'
#' #show the plot
#' barchart_normalized
#'
bar_chart_normalized <- function(data, cat, series, series_labels = series){
  if(length(cat) == 1){
    cat <- data[,cat]
  }
  df <- normalize_rows(data, cat, series)
  y_end <- get_margins()$top + 24*length(cat)
  svg_string <- initialize(y_vector = cat,
             bar_width = 16) %>%
    draw_bars_basic(df, cat, series, series_labels, df_with_real_values = data) %>%
    paste(add_vertical_index(get_margins()$left + 200, (y_end+16+4.8-24)),
          draw_rect(get_margins()$left + 200 + 5,  get_margins()$top, "white", 25, y_end, style = "total_white"), #it covers the sum labels
          sep='\n') %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}

#' Generate bar chart with absolute variance.
#'
#' Visualize variance between baseline and real in absolute units. Choose colors parameter accordingly to business interpretation of larger/smaller values.
#'
#' @inheritParams column_chart_absolute_variance
#' @inheritParams bar_chart
#' @param y_title title of the series values
#' @param y_style style of y axis to indicate baseline scenario
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#'
#' # get some data
#' real <- sin(1:5)
#' baseline <- cos(1:5)
#' cat <- letters[1:5]
#'
#' bar_chart_absolute_variance(
#'   cat = cat,
#'   baseline = baseline,
#'   real = real,
#'   y_title = 'a title')
bar_chart_absolute_variance <-
  function(data = NULL,
           cat,
           baseline,
           real,
           colors = 1,
           y_title,
           y_style = 'previous') {

    if (!is.null(data)) {
      cat <- get_vector(data, cat)
      baseline <- get_vector(data, baseline)
      real <- get_vector(data, real)
    }

    variance <- real - baseline

    width_of_one <-
      200 / max(abs(real), abs(baseline)) # units in variance plot must be the same as in the normal plot


    #dealing with negative values
    neg <- variance[variance < 0]
    if (length(neg) == 0)
      shift <- 0
    else
      shift <- width_of_one * abs(min(neg)) + 35 # 35 px for labels


    svg_string <- initialize(y_vector = cat, bar_width = 16, width = shift + 200 + get_margins()$left) %>%
      draw_bars_variance(cat, variance, width_of_one, shift, colors, y_title, y_style) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }


draw_bars_variance <-
  function(svg_string,
           cat,
           variance,
           width_of_one,
           shift,
           colors,
           y_title,
           y_style) {


    y <- get_margins()$top


    data <- data.frame(variance)
    colnames(data) <- y_title

    color <- choose_variance_colors(colors)
    curr_color <- list()
    curr_color$bar_color <-
      choose_waterfall_color(variance[1], color$pos_color, color$neg_color)
    svg_string <- paste(
      svg_string,
      add_bar_basic(
        shift,
        data,
        cat,
        y_title,
        1,
        y,
        width_of_one,
        y_title,
        color = curr_color,
        ax_style = y_style,
        show_series_labels = T,
        neg_pos = 'left'
      ),
      sep = '\n'
    )
    y <- y + 24
    for (i in 2:length(cat)) {
      curr_color$bar_color <-
        choose_waterfall_color(variance[i], color$pos_color, color$neg_color)
      svg_string <- paste(
        svg_string,
        add_bar_basic(
          shift,
          data,
          cat,
          y_title,
          i,
          y,
          width_of_one,
          color = curr_color,
          ax_style = y_style,
          show_series_labels = F,
          neg_pos = 'left'
        ),
        sep = '\n'
      )
      y <- y + 24
    }
    return(svg_string)
  }

#' Generate bar chart with relative variance (in percents).
#'
#' @inheritParams bar_chart_absolute_variance
#' @param styles optional vector with styles of the pin heads
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' # get some data
#' real <- sin(1:5)
#' baseline <- cos(1:5)
#' cat <- letters[1:5]
#'
#' bar_chart_relative_variance(
#'   cat = cat,
#'   baseline = baseline,
#'   real = real,
#'   y_title = 'a title')
bar_chart_relative_variance <-
  function(data = NULL,
           cat,
           baseline,
           real,
           colors = 1,
           y_title,
           y_style = 'previous',
           styles = NULL) {

    if (!is.null(data)) {
      cat <- get_vector(data, cat)
      baseline <- get_vector(data, baseline)
      real <- get_vector(data, real)
    }

    values <- real / baseline * 100 - 100
    width_of_one <- 2 # units in relative variance plot must be the same in all variance plots

    #dealing with negative values
    neg <- values[values < 0]
    if (length(neg) == 0)
      shift <- 0
    else
      shift <- width_of_one * abs(min(neg)) + 25 # 25 px for value labels


    svg_string <- initialize(y_vector = cat, bar_width = 16, width = shift + 200 + get_margins()$left) %>%
    draw_pins_variance(cat, values, width_of_one, shift, colors, y_title, y_style, styles) %>%
    finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }


draw_pins_variance <- function(svg_string, cat, values, width_of_one, shift, colors, y_title, y_style, styles){

  y <- get_margins()$top

  data <- data.frame(values)
  colnames(data) <- y_title

  color <- choose_variance_colors(colors)
  curr_color <- list()
  curr_color$bar_color <-
    choose_waterfall_color(values[1], color$pos_color, color$neg_color)

  svg_string <- paste(
    svg_string,
    add_pin(
      shift,
      data[1,],
      cat[1],
      y,
      width_of_one,
      series_labels = y_title,
      color = curr_color,
      ax_style = y_style,
      show_series_labels = T,
      style = styles[1]
    ),
    sep = '\n'
  )
  y <- y + 24
  for (i in 2:length(cat)) {
    curr_color$bar_color <-
      choose_waterfall_color(values[i], color$pos_color, color$neg_color)
    svg_string <- paste(
      svg_string,
      add_pin(
        shift,
        data[i,],
        cat[i],
        y,
        width_of_one,
        color = curr_color,
        ax_style = y_style,
        show_series_labels = F,
        style = styles[i]
      ),
      sep = '\n'
    )
    y <- y + 24
  }
  return(svg_string)
}
