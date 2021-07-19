library(magrittr) # pipes
library(docstring)
source(file.path("utils", "drawing_utils_K.R"))
#source(file.path("utils", "chart_utils_K.R"))
source(file.path("utils", "chart_utils.R"))
source(file.path("utils", "drawing_utils.R"))

add_bar_grouped <-
  function(shift,
           data,
           cat,
           series,
           k ,
           y,
           width_of_one,
           series_labels = NULL,
           df_styles = NULL) {
    #k - points to the row in data frame
    #df_with_real_values - to show real values after the normalization
  
    svg_string <- ""
    labels <- ""
    value_label <- ""
    x <- 80 + shift
    for (i in length(series):1){ #going through series
      value <- data[,series[i]] #a vector
      color <- get_gray_color_stacked(i)
      styles <- df_styles[,series[i]]
      #if(is.null(series_labels)==FALSE){
      #  if(length(series)>1){ #if there is only one series, no series label needed
      #    labels <- paste(labels, 
                          #series label
      #                    add_label((x+(width_of_one*(value[k])/2)), y+12-16, series_labels[i]),
      #                    sep='\n')}
      #}
      
      if(i != 1 && value[k] < 0){
          x <- x - (width_of_one*abs(value[k]))
      }
      #first element in series defines the triangle markers
      if(i == 1){
        rect <- draw_triangle("", width_of_one*value[k] + x ,y+8 , orientation = "bottom", style=styles[k])
      }else{
        rect <- draw_rect(x, y - 4.8*(i-2), color$bar_color, (width_of_one*abs(value[k])), 16, style = styles[k])
      }
      x <- 80 + shift
      
      #value_text <- value[k]
      #checking if there's enough place for a label
      #if(str_width(abs(value_text))+3.2 < abs(value_text)*width_of_one && length(series) > 1){
        
        #labels <- paste(
          #labels,
          #each series value label
          #add_label((x+(width_of_one*(abs(value[k]))/2)), y+12, value_text, color=color$text_color),
          #sep='\n'
        #)
      #}
      svg_string <- paste(svg_string, rect, labels,  sep = '\n')
      #if(value[k] > 0){ x <- x + (width_of_one*value[k])}
      
      if(i == 2){
        if(value[k]>0){value_label <- paste(value_label,
                                            add_label((x + width_of_one*value[k]+4.8), y+12, value[k], anchor="start"),
                                            sep='\n')}
        else{value_label <- add_label(x + 4.8, y+12, value[k], anchor="start")}
      }
      
    }
    
    return(paste(svg_string,
                 # value label
                 #add_label((x+4.8), y+12,all_sums[k], anchor="start"),
                 value_label,
                 #category label
                 add_label( 72.2, y+14, cat[k], anchor="end"),
                 #vertical axis
                 draw_line(80+shift, 80+shift,(y-4.8), (y+16+4.8)),
                 #labels,
                 sep = '\n'
    ))
  }

draw_bars_grouped <- function(svg_string, data, cat, series, series_labels, df_with_real_values=NULL, df_styles = NULL){
  bars <- svg_string
  y = 50
  maxes <- c()
  neg <- c()
  #looking for the maximum value
  for(k in 1:(length(series))){
    maxes <- c(maxes, max(abs(data[,series[k]])))
    neg <- c(neg, data[,series[k]][data[,series[k]]<0])
  }
  maximum <- max(maxes)
  width_of_one <- 200/maximum
  
  #dealing with negative values
  shift <- width_of_one*abs(min(neg))
  if(is.finite(shift)==FALSE){shift <- 0} #in case there are no negative values
  bars <- paste(bars,
                add_bar_grouped(shift, data,cat, series,1, y, width_of_one, series_labels, df_styles=df_styles),
                sep='\n')
  y <- y+24
  for(i in 2:length(cat)){
    bars <- paste(bars,
                  add_bar_grouped(shift ,data,cat, series,i, y, width_of_one, df_styles = df_styles),
                  sep='\n')
    y <- y+24
  }
  return (bars)
}


#' Generates grouped horizontal barchart with scenario triangles.  
#'
#' @param data data frame containing data to be plotted
#' @param cat vector cointaining category names of values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param df_styles optional data frame containing styles of bars. It is necessary that df_style has the same column names as defined in series vector.
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
barchart_plot_grouped <- function(data, cat, series, series_labels, df_styles = NULL){
  # TODO all values in one bar should have the same sign
  initialize() %>%
    draw_bars_grouped(.,data, cat, series, series_labels, df_styles = df_styles) %>%
    finalize()
}


#--- test ---
#data <- data.frame(
#  city = c("Berlin", "Munich", "Cologne", "London", "Vienna", "Paris", "Zurich", "Rest"),
#  products = c(538, 250, -75, 301,227,90, 40, 269),
#  services = c(621,545,-302, 44,39,30,34,333),
#  triangles = c(600,600,-302, 600,600,30,600,600)
#)
#series <- c("triangles","products", "services")

#df <- data.frame(
#  animal = c("cat", "doggo", "rabbito"),
#  hungry = c(7,5,9),
#  relaxed = c(3,4,5),
#  wounded = c(1,8,5)
#)
#srs <- c("hungry", "relaxed", "wounded")

#df_styles <- data.frame(
#  products = c(rep('plan',8)),
#  services = c(rep('actual', 8)),
#  triangles = c(rep('plan', 8))
#)
#styles <- c(rep('actual',6), 'forecast', 'actual')

#barchart_plot_grouped(data, data$city, series, series, df_styles) %>% show()









