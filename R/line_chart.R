

#system jest taki ze jest druga ramka danych z dwoma kolumnami ktora zawiera nazwe serii i wspolrzedne punktu ktory ma byc wyswietlony

draw_circle_lineplot <- function(x,y, color){
  #<circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red" />
  return(paste0('<circle cx="',
                x,
                '" cy="',
                y,
                '" r="2.4"  fill="',
                color,
                '" stroke="',
                color,
                '" />'))}
#---
#adding category names, x-axis, assisting lines
add_category <- function(shift, data, cat, x, k, cat_width){ #cat jest calym wektorem
  return(paste(
    #x-axis line
    draw_line(x - cat_width/2, x + cat_width/2, 250, 250),
    #label with the value
    add_label(x, 268.4 + shift, cat[k]),
    #asisting line
    draw_line(x, x, 50, 250+shift, "black", 0.1),
    sep="\n"
    ))

}

#----
draw_lines <- function(svg_string, data, cat, series, series_labels, ser_names, point_cords, cat_width){

  labels <-""
  lines <- svg_string
  #colors <- c("rgb(64,64,64)","rgb(166,166,166)","rgb(70,70,70)","rgb(90,90,90)" , "rgb(110,110,110)","rgb(127,127,127)" )
  x = 80
  maxes <- c()
  neg <- c()

  for(k in 1:(length(series))){
    maxes <- c(maxes, max(abs(data[,series[k]])))
    neg <- c(neg, data[,series[k]][data[,series[k]]<0])
  }
  maximum <- max(maxes)
  height_of_one <- 200/maximum
  #calculating the shift
  shift <- height_of_one*abs(min(ifelse(is.null(neg),0,neg)))
  if(is.finite(shift)==FALSE){shift <- 0} #in case there are no negative values

  for(k in 1:(length(series))){ #going through series
    #color <- colors[k]
    color <- get_gray_color_stacked(k)$bar_color
    values <- data[, series[k]]
    labels <- paste(labels,
                    add_label(75.2, 250- height_of_one*values[1] +6, series_labels[k],anchor="end"),
                    sep="\n"

    )


    for(i in 1:(length(cat)-1)){ #going through categories
      lines <- paste(lines,
                     draw_line(x, x + cat_width, 250-(height_of_one*values[i]), 250-(height_of_one*values[i+1]), color),
                     add_category(shift, data, cat, x, i, cat_width),
                     sep='\n')
      x <- x + cat_width
    }
    j <- length(cat)
    if (k == 1) {
      lines <- paste(lines,
                     add_category(shift, data, cat, x, j, cat_width),
                     sep='\n')
    }
    x <- 80
  }

  chosen_points <- draw_chosen_points(data, series, height_of_one, ser_names, point_cords, cat_width)
  return (paste(lines, labels, chosen_points, sep='\n'))
}


#drawing the point we have to have highlighted on the plot
draw_chosen_points <- function(data, series, height_of_one, ser_names, point_cords, cat_width){
  chosen_points <- ""
  for(i in 1:length(ser_names)){
    #calculating the x cordinates
    x <- 80 + cat_width*(point_cords[i]-1)
    y <- 250 - height_of_one*data[, ser_names[i]][point_cords[i]]
    #circle_color <- colors[match(ser_names[i], series)[1]]
    circle_color <- get_gray_color_stacked(match(ser_names[i], series)[1])$bar_color
    chosen_points <- paste(chosen_points,
                           draw_circle_lineplot(x,y,circle_color),
                           #label
                           add_label(x, y - 4.8 - 2.4, data[, ser_names[i]][point_cords[i]], "black"),
                           sep='\n')
  }
  return(chosen_points)
}

#----
#' Generates a line plot with markers on chosen points. Allows only one point per time interval. To create a plot with many points within one time interval try line_plot_with_many_points_complex().
#'
#' @param data data frame containing data to be plotted
#' @param x vector cointaining time interwals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param ser_names vector containing column names of a value to be marked
#' @param point_cords vector of the same length as ser_names containing numerical values of indexes in data of values to be marked
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
#'
#' #preparing the data frame
#' data <- data.frame(
#' time = c(2015, 2016, 2017, 2018, 2019, 2020),
#' Gamma = c(98, 80, 16, 25, 55, 48),
#'Delta = c(22, 25, 67, 73, 102, 98)
#')
#'
#' #defining rest of arguments
#' names <- c("Gamma", "Gamma", "Gamma","Gamma","Delta", "Delta")
#' cords <- c(1, 4,5,2, 5,4)
#'
#' #generating SVG string
#' line_chart  <- line_chart(
#'   data = data,
#'   x = data$time,
#'   series = c("Gamma", "Delta"),
#'   series_labels =c("Gamma inc.", "Delta inc."),
#'   ser_names = names,
#'   point_cords = cords,
#'   interval = "years")
#'
#' #showing the plot
#' line_chart %>% SVGrenderer
#'
line_chart <- function(data, x, series, series_labels, ser_names, point_cords, interval="months"){

  if(length(x) == 1){
    x <- data[,x]
  }
  cat_width <- get_interval_width(interval)$category_width
  initialize() %>%
  draw_lines(data, x, series, series_labels, ser_names, point_cords, cat_width) %>%
  finalize()
}
