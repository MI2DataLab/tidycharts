
#---

add_marker_normalized <- function(data, cat, value, x, height_of_one, k, y, show_label,if_rect, cat_width){ #cat jest calym wektore, k to numer serii w ktorej jestesmy y - przesuniecie

  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  #we do not draw the rectangle markers on the top
  if(if_rect==1){rect<-draw_rect(x-2.4, (top_margin+197.6-(height_of_one*value))-y, "rgb(127,127,127)", 4.8, 4.8)}
  else(rect<-"")

  # if(is.na(show_label) == FALSE){
  #value_label <- add_label(x, 250-y - (height_of_one*value/2) +6, value,get_gray_color_stacked(k)$text_color)
  if(is.na(show_label) == FALSE){
    value_label <- add_label(x, top_margin+200-y - (height_of_one*value/2) +6, value, get_color_stacked(k)$text_color )
  }else{
    value_label<-""
  }
  # }else{
  #   value_label<-""
  # }

  return(paste(rect,
    #asisting line
    draw_line(x, x, top_margin, top_margin+200, "white", 0.1),
    #value label
    #add_label(x,250-y - (height_of_one*value/2) +6, value,"white"),
    value_label,
    sep="\n"

  ))

}

#----

draw_polygons_normalized <- function(svg_string, data, cat, series, series_labels, show_labels, cat_width){

  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  polygons <- svg_string
  x = left_margin
  labels <- ""
  #markers need to be separated so the polygons wont overlap them
  markers <- ""
  all_sums <- rowSums(data[,series])
  y <- rep(0, length(cat))
  height_of_one <- 200/all_sums[1]
  #-----
  for(k in 1:(length(series))){ #going through series
    color <- get_color_stacked(k)$bar_color
    values <- data[, series[k]]
    #series labels
    labels <- paste(labels,
                    add_label(left_margin-4.8, top_margin+200-y[1] - (height_of_one*values[1]/2), series_labels[k], anchor="end"),
                    sep="\n"

    )

    for(i in 1:(length(cat)-1)){ #going throug categories
      #defining if its the top level or not, deciding if the markers are gonna show
      if(k == length(series)){
        if_rect <- 0
      }else{if_rect<-1}
      if (k == 1) {
        polygons <- paste(polygons,
                          #adding ticks
                          draw_line(x, x, top_margin+200, top_margin+201.6),
                          #x-axis line
                          draw_line(x - cat_width/2, x + cat_width/2, top_margin+200, top_margin+200),
                          #category label
                          add_label(x, top_margin+218.4, cat[i]),
                          sep = '\n')
      }
      height_of_one <- 200/all_sums[i]
      polygons <- paste(polygons,
                        draw_quadrangle(x, (top_margin+200-(height_of_one*values[i])) - y[i],
                                        x + cat_width, (top_margin+200-(200/all_sums[i+1]*values[i+1])) - y[i+1],
                                        x + cat_width, top_margin+200 - y[i+1],
                                        x, top_margin+200 - y[i],
                                        color),
                        sep='\n')

      markers <- paste(markers, add_marker_normalized(data, cat[i], values[i], x, height_of_one, k, y[i], show_labels[i], if_rect, cat_width), sep='\n')
      x <- x + cat_width
      y[i] <- y[i] + height_of_one*values[i]
    }
    j <- length(cat)
    height_of_one <- 200/all_sums[j]
    if (k == 1) {
      polygons <- paste(polygons,
                        #adding ticks
                        draw_line(x, x ,top_margin+200, top_margin+201.6),
                        #x-axis line
                        draw_line(x - cat_width/2, x + cat_width/2, top_margin+200, top_margin+200),
                        #category label
                        add_label(x, top_margin+218.4, cat[j]),
                        sep = '\n')
    }
    polygons <- paste(polygons,
                      add_marker_normalized(data, cat[j], values[j], x, height_of_one, j, y[j], show_labels[j], if_rect, cat_width),
                      sep='\n')
    x <- left_margin
    y[j] <- y[j] + height_of_one*values[j]

  }
  x <- left_margin
  return (paste(polygons, markers,labels, add_index(left_margin + cat_width*(length(cat)-1), top_margin),  sep='\n'))
}

#----
#' Generates normalized areas (stacked lines) plot. If more than one series is supplied, stacked areas plot is generated.
#'
#' @param data data frame containing data to be plotted
#' @param x vector containing time intervals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param show_labels vector of the same length as cat containing NA or not NA values defining which categories should have labels of values displayed
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#'
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#'
#'
#' #preparing data frames
#' data <- data.frame(
#' weeks =    c(28,   29, 30,  31,  32,  33,  34,  35,  36, 37),
#' Services = c(130,150, 182, 170, 170, 140, 130, 130, 135, 140),
#' Software = c(100, 88, 83,   90, 92,   95, 129, 130, 130, 135),
#' Products = c(20,  35, 36,    40, 22,  25, 24,   19,  36,  40)
#')
#'
#'#defining the rest of the arguments
#' series <- c("Software", "Services", "Products")
#' labels <- c(NA, 1, NA, 1, NA, NA, 1, NA, 1, NA)
#'
#' #generating the SVG string
#' line_chart_normalized <- line_chart_normalized(data, data$weeks, series, series, labels, "weeks")
#'
#' #show the plot
#' line_chart_normalized
line_chart_normalized <- function( data, x, series, series_labels, show_labels, interval="months"){

  cat_width <- get_interval_width(interval)$category_width
  svg_string <- initialize() %>%
    draw_polygons_normalized(data, x, series, series_labels,show_labels, cat_width) %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}

