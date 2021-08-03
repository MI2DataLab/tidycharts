

#adding a rectangle marker with label above or under + categories labels
add_point <- function(shift ,data, cat, value, x, height_of_one, color,k, minimal, cat_width, style = NULL){ #cat jest calym wektore, k to numer serii w ktorej jestesmy

  label<-""
  if(value > 0){
    rect <- draw_rect(x-5.6, (244.4-(height_of_one*value)), color, 11.2, 11.2, style = style)
    y_label <- (250-(height_of_one*value))
    }
  else{
    rect <- draw_rect(x-5.6, (250-5.6 +(height_of_one*abs(value))), color, 11.2, 11.2, style = style)
    y_label <- (250 + (height_of_one*abs(value)))
    }

  if(minimal==1){ #ostatni podpis pod spodem
    #label under
    label <- add_label(x, y_label + 5.6 + 4.8 +9, value, color) #?
  }else{
    #label above
    label <- add_label(x, y_label -5.6 - 4.8, value, color)
  }

  return(paste(
      #marker
      rect,
      #label with the marker value
      label,
      sep="\n"

    ))

}

find_height <- function(data, series){
  maxes <- c()
  for(k in 1:(length(series))){
    maxes <- c(maxes, max(data[,series[k]]))
  }
  maximum <- max(maxes)
  height_of_one <- 200/maximum
  return(height_of_one)
}


#----
draw_points <- function(svg_string, data, cat, series, series_labels, cat_width, styles = NULL){

  points <- svg_string
  labels <- ""
  colors <- c("rgb(64,64,64)","rgb(166,166,166)","rgb(70,70,70)","rgb(90,90,90)" , "rgb(110,110,110)","rgb(127,127,127)" )
  x <- 80
  #TODO tu jest redundancja kodu
  # zeby znalezc takie maksimum mozna wywolac max(data[series])
  maxes <- c()
  averages <- c()
  neg <- c()
  #looking for the maximum value and minimum average
  for(k in 1:(length(series))){
    maxes <- c(maxes, max(abs(data[,series[k]])))
    averages <- c(averages, mean(data[,series[k]]))
    neg <- c(neg, data[,series[k]][data[,series[k]]<0])
  }
  maximum <- max(maxes)
  min_avg <- min(averages)
  height_of_one <- 200/maximum
  #calculating the shift
  shift <- height_of_one*abs(min(neg)) + 12 + 4.8
  if(is.finite(shift)==FALSE){shift <- 0} #in case there are no negative values



  for(k in 1:(length(series))){ #going through series

    if(mean(data[,series[k]])==min_avg){
      minimal <- 1
    }else{
      minimal<-0
    }

    color <- colors[k]
    values <- data[, series[k]]
    labels <- paste(labels, add_label(80-4.8 - 5.6, 250- height_of_one*values[1]+3, series_labels[k], anchor="end"), sep='\n')
    for(i in 1:(length(cat)-1)){ #drawing each point
      style <- styles[i, k]
      if (k == 1) {
        points <- paste(points,
                        #category label
                        add_label(x, 268.4 + shift, cat[i], "black"),
                        #ticks
                        draw_line(x, x, 250,251.6),
                        #x-axis line
                        draw_line(x - cat_width/2, x + cat_width/2, 250, 250),
                        sep = '\n')
      }
      points <- paste(points,
                      add_point(shift, data, cat[i], values[i], x, height_of_one, color,k, minimal, cat_width, style = style),
                      #line between two points
                      draw_line(x+5.6, x + cat_width, (250-(height_of_one*values[i])), (250-(height_of_one*values[i+1])),color),
                      sep='\n')

      x <- x + cat_width
    }
    i <- length(cat)
    style <- tail(styles, n=1)[[k]]
    if(k == 1){
      points <- paste(points,
                      #category label
                      add_label(x, 268.4 + shift, cat[i], "black"),
                      #ticks
                      draw_line(x, x, 250,251.6),
                      #x-axis line
                      draw_line(x - cat_width/2, x + cat_width/2, 250, 250),
                      sep = "\n")
    }
    points <- paste(points,
                    add_point(shift, data, cat[i], values[i], x, height_of_one, color, k, minimal, cat_width, style = style ),
                    sep='\n')
    x <-80
  }


  return (paste(points, labels, sep='\n'))
}

#----

#line with few data points
#' Generates line plot with markers on every value.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector cointaining time interwals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#' @param styles optional data frame with style names. Styles of the markers will be plotted accordingly.
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
line_plot <- function(data, cat, series, series_labels, interval="months", styles = NULL){ #interval <- week, month, quarter, year

  #if(interval == "week"){cat_width <- 32}
  #if(interval == "month"){cat_width <- 48}
  #if(interval == "quarter"){cat_width <- 56}
  #if(interval == "year"){cat_width <- 32}
  cat_width <- get_interval_width(interval)$category_width
  maxes <- c()
  averages <- c()
  neg <- c()
  #looking for the maximum value and minimum average
  for(k in 1:(length(series))){
    maxes <- c(maxes, max(abs(data[,series[k]])))
    averages <- c(averages, mean(data[,series[k]]))
    neg <- c(neg, data[,series[k]][data[,series[k]]<0])
  }
  maximum <- max(maxes)
  min_avg <- min(averages)
  height_of_one <- 200/maximum
  #calculating the shift
  shift <- height_of_one*abs(min(neg)) + 12 + 4.8
  if(is.finite(shift)==FALSE){shift <- 0} #in case there are no negative values

    initialize(width = 80+ cat_width*length(cat) + 80, height = 250+shift + 20) %>%
    draw_points(.,data, cat, series, series_labels, cat_width, styles) %>%
    finalize() #%>% show()
}


#' Generates line plot with markers on every value with index on a given value.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector cointaining time interwals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#' @param index_val numeric value of the index
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
line_plot_index <- function(data, cat, series, series_labels, index_val, interval = "months"){
  #height_of_one <- find_height(data, series)
  cat_width <- get_interval_width(interval)$category_width
  maxes <- c()
  averages <- c()
  neg <- c()
  #looking for the maximum value and minimum average
  for(k in 1:(length(series))){
    maxes <- c(maxes, max(abs(data[,series[k]])))
    averages <- c(averages, mean(data[,series[k]]))
    neg <- c(neg, data[,series[k]][data[,series[k]]<0])
  }
  maximum <- max(maxes)
  min_avg <- min(averages)
  height_of_one <- 200/maximum
  #calculating the shift
  shift <- height_of_one*abs(min(neg)) + 12 + 4.8
  if(is.finite(shift)==FALSE){shift <- 0} #in case there are no negative values

  initialize(width = 80+ cat_width*length(cat) + 80, height = 250+shift + 20) %>%
    draw_points(.,data, cat, series, series_labels, cat_width) %>%
    paste(.,
          add_index(80 +cat_width/2 + cat_width*(length(cat)-1), 250-height_of_one*index_val),
          sep='\n') %>%
    finalize() #%>% show()
}

