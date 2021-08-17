

#adding a rectangle marker with label above or under + categories labels
add_point <- function(shift, data, x, value, x_pos, height_of_one, color, k, minimal, cat_width, style = NULL){ #cat jest calym wektore, k to numer serii w ktorej jestesmy

  label<-""
  if(value > 0){
    rect <- draw_rect(x_pos-5.6, (244.4-(height_of_one*value)), color, 11.2, 11.2, style = style)
    y_label <- (250-(height_of_one*value))
    }
  else{
    rect <- draw_rect(x_pos-5.6, (250-5.6 +(height_of_one*abs(value))), color, 11.2, 11.2, style = style)
    y_label <- (250 + (height_of_one*abs(value)))
    }

  if(minimal==1){ #ostatni podpis pod spodem
    #label under
    label <- add_label(x_pos, y_label + 5.6 + 4.8 +9, value, color) #?
  }else{
    #label above
    label <- add_label(x_pos, y_label -5.6 - 4.8, value, color)
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
draw_points <- function(svg_string, data, x, series, series_labels, cat_width, styles = NULL, height_of_one, min_avg, shift){

  points <- svg_string
  labels <- ""
  x_pos <- 80

  for(k in 1:(length(series))){ #going through series

    if(mean(data[,series[k]]) == min_avg){
      minimal <- 1
    }else{
      minimal<-0
    }

    #color <- colors[k]
    color <- get_gray_color_stacked(k)$bar_color
    values <- data[, series[k]]
    labels <- paste(labels, add_label(80 - 4.8 - 5.6, 250- height_of_one*values[1]+3, series_labels[k], anchor="end"), sep='\n')
    for(i in 1:(length(x)-1)){ #drawing each point
      style <- styles[i, k]
      if (k == 1) {
        points <- paste(points,
                        #category label
                        add_label(x_pos, 268.4 + shift, x[i], "black"),
                        #ticks
                        draw_line(x_pos, x_pos, 250,251.6),
                        #x-axis line
                        draw_line(x_pos - cat_width/2, x_pos + cat_width/2, 250, 250),
                        sep = '\n')
      }
      points <- paste(points,
                      add_point(shift, data, x[i], values[i], x_pos, height_of_one, color,k, minimal, cat_width, style = style),
                      #line between two points
                      draw_line(x_pos+5.6, x_pos + cat_width, (250-(height_of_one*values[i])), (250-(height_of_one*values[i+1])),color),
                      sep='\n')

      x_pos <- x_pos + cat_width
    }
    i <- length(x)
    style <- utils::tail(styles, n=1)[[k]]
    if(k == 1){
      points <- paste(points,
                      #category label
                      add_label(x_pos, 268.4 + shift, x[i], "black"),
                      #ticks
                      draw_line(x_pos, x_pos, 250,251.6),
                      #x-axis line
                      draw_line(x_pos - cat_width/2, x_pos + cat_width/2, 250, 250),
                      sep = "\n")
    }
    points <- paste(points,
                    add_point(shift, data, x[i], values[i], x_pos, height_of_one, color, k, minimal, cat_width, style = style ),
                    sep='\n')
    x_pos <-80
  }


  return (paste(points, labels, sep='\n'))
}

#----

#line with few data points
#' Generates line plot with markers on every value.
#'
#' @param data data frame containing data to be plotted
#' @param x vector cointaining time interwals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#' @param styles optional data frame with style names. Styles of the markers will be plotted accordingly.
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
#'
#' #preparing a data frame
#' data <- data.frame(
#' time = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
#' PL = (c(51, 42, 50, 58, 78, 79) - 30),
#' AC = (c(62, 70, 67, 77, 63, 62) - 30)
#' )
#' #preparing the styles data frame
#' styles <- data.frame(
#'  PL = c("plan", "plan", "plan", "plan", "plan", "plan"),
#'  AC = c("actual", "actual", "actual", "forecast", "forecast", "forecast")
#' )
#'
#' #generating svg string
#' line_chart <- line_chart_markers(data, data$time, c("PL", "AC"), c("PL", "AC"),"months", styles)
#'
#' #show the plot
#' line_chart %>% SVGrenderer()
#'
#'
line_chart_markers <- function(data, x, series, series_labels, interval="months", styles = NULL){ #interval <- week, month, quarter, year

  if(length(x) == 1){
    x <- data[,x]
  }
  cat_width <- get_interval_width(interval)$category_width
  averages <-rowMeans(data[series])
  maximum <- max(abs(data[, series]))
  neg <- data[, series][data[,series] < 0]
  min_avg <- min(averages)
  height_of_one <- 200/maximum

  #calculating the shift
  if(length(neg) == 0){shift <- 0}
  else{shift <- height_of_one*abs(min(neg)) + 12 + 4.8}

  initialize(width = 80+ cat_width*length(x) + 80, height = 250 + shift + 20) %>%
    draw_points(data, x, series, series_labels, cat_width, styles, height_of_one, min_avg, shift) %>%
    finalize()
}


#' Generates line plot with markers on every value with index on a given value.
#'
#' @param data data frame containing data to be plotted
#' @param x vector containing time intervals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#' @param ref_val numeric value of the index
#' @param ref_label string defining a text that should be displayed in the referencing line. Set by default to index_val.
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#' @param styles optional data frame with style names. Styles of the markers will be plotted accordingly.
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
#'
#' #preparing a data frame
#' data <- data.frame(
#' time = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
#' PL = (c(51, 42, 50, 58, 78, 79) - 30),
#' AC = (c(62, 70, 67, 77, 63, 62) - 30)
#' )
#' #preparing the styles data frame
#' styles <- data.frame(
#'  PL = c("plan", "plan", "plan", "plan", "plan", "plan"),
#'  AC = c("actual", "actual", "actual", "forecast", "forecast", "forecast")
#' )
#'
#' #generating svg string
#' line_chart_ref <- line_chart_markers_reference(
#'   data = data,
#'   x = data$time,
#'   series = c("PL", "AC"),
#'   series_labels = c("PL", "AC"),
#'   ref_val = 42,
#'   ref_label = "index",
#'   styles=styles)
#'
#' #show the plot
#' line_chart_ref %>% SVGrenderer()
#'
line_chart_markers_reference <- function(data, x, series, series_labels, ref_val, ref_label=ref_val, interval = "months", styles=NULL){

  if(length(x) == 1){
    x <- data[,x]
  }
  cat_width <- get_interval_width(interval)$category_width
  averages <-rowMeans(data[,series])
  maximum <- max(abs(data[, series]))
  neg <- data[, series][data[,series] < 0]
  min_avg <- min(averages)
  height_of_one <- 200/maximum

  #calculating the shift
  if(length(neg) == 0){shift <- 0}
  else{shift <- height_of_one*abs(min(neg)) + 12 + 4.8}

  initialize(width = 80 + cat_width*length(x) + 80, height = 250+shift + 20) %>%
    draw_points(data, x, series, series_labels, cat_width, styles, height_of_one, min_avg, shift) %>%
    paste(add_index(80 +cat_width/2 + cat_width*(length(x)-1), 250-height_of_one*ref_val, ref_label),
          sep='\n') %>%
    finalize()

}


