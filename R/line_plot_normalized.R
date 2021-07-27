
#---

add_marker_normalized <- function(data, cat, value, x, height_of_one, k, y, show_label,if_rect, cat_width){ #cat jest calym wektore, k to numer serii w ktorej jestesmy y - przesuniecie

  #we do not draw the rectangle markers on the top
  if(if_rect==1){rect<-draw_rect(x-2.4, (247.6-(height_of_one*value))-y, "rgb(127,127,127)", 4.8, 4.8)}
  else(rect<-"")

  # if(is.na(show_label) == FALSE){
  #value_label <- add_label(x, 250-y - (height_of_one*value/2) +6, value,get_gray_color_stacked(k)$text_color)
  if(is.na(show_label) == FALSE){
    value_label <- add_label(x, 250-y - (height_of_one*value/2) +6, value, get_gray_color_stacked(k)$text_color )
  }else{
    value_label<-""
  }
  # }else{
  #   value_label<-""
  # }

  return(paste(rect,
    #asisting line
    draw_line(x, x, 50,250, "white", 0.1),
    #category label
    add_label(x, 268.4, cat),
    #value label
    #add_label(x,250-y - (height_of_one*value/2) +6, value,"white"),
    value_label,
    #adding ticks
    draw_line(x, x ,250, 251.6),
    #x-axis line
    draw_line(x - cat_width/2, x + cat_width/2, 250, 250),
    sep="\n"

  ))

}

#----

draw_polygons_normalized <- function(svg_string, data, cat, series, series_labels, show_labels, cat_width){

  polygons <- svg_string
  x = 80
  labels <- ""
  #markers need to be separated so the polygons wont overlap them
  markers <- ""
  all_sums <- rowSums(data[,series])
  y <- rep(0, length(cat))
  height_of_one <- 200/all_sums[1]
  #-----
  for(k in 1:(length(series))){ #going through series
    color <- get_gray_color_stacked(k)$bar_color
    values <- data[, series[k]]
    #series labels
    labels <- paste(labels,
                    add_label(75.2, 250-y[1] - (height_of_one*values[1]/2), series_labels[k], anchor="end"),
                    sep="\n"

    )

    for(i in 1:(length(cat)-1)){ #going throug categories
      #defining if its the top level or not, deciding if the markers are gonna show
      if(k == length(series)){
        if_rect <- 0
      }else{if_rect<-1}
      height_of_one <- 200/all_sums[i]
      polygons <- paste(polygons,
                        draw_quadrangle(x, (250-(height_of_one*values[i])) - y[i],
                                        x + cat_width, (250-(200/all_sums[i+1]*values[i+1])) - y[i+1],
                                        x + cat_width, 250 - y[i+1],
                                        x, 250 - y[i],
                                        color),
                        sep='\n')

      markers <- paste(markers, add_marker_normalized(data, cat[i], values[i], x, height_of_one, k, y[i], show_labels[i], if_rect, cat_width), sep='\n')
      x <- x + cat_width
      y[i] <- y[i] + height_of_one*values[i]
    }
    j <- length(cat)
    height_of_one <- 200/all_sums[j]
    polygons <- paste(polygons,
                      add_marker_normalized(data, cat[j], values[j], x, height_of_one, j, y[j], show_labels[j], if_rect, cat_width),
                      sep='\n')
    x <- 80
    y[j] <- y[j] + height_of_one*values[j]

  }
  x <- 80
  return (paste(polygons, markers,labels, add_index(80 + cat_width*(length(cat)-1), 50),  sep='\n'))
}

#----
#' Generates normalized areas (stacked lines) plot. If more than one series is supplied, stacked areas plot is generated.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector cointaining time interwals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param show_labels vector of the same length as cat containg NA or not NA values defining which categories should have labels of values displayed
#'
#' @returnSVG SVg string containing chart
#' @export
#'
#' @examples
line_plot_normalized <- function( data, cat, series, series_labels, show_labels, interval="months"){

  cat_width <- get_interval_width(interval)$category_width
  initialize() %>%
    draw_polygons_normalized(.,data, cat, series, series_labels,show_labels, cat_width) %>%
    finalize()
}

#tests
data <- data.frame(
  months = c("Jan", "Feb", "Mar", "Apr"),
  cos = c(4,5,4,6),
  cosiek = c(2, 3, 3.5, 1)
)
series <- c("cos", "cosiek")

line_plot_normalized(data, data$months, series, series ,c(NA,1,1,NA),"years") %>% SVGrenderer()
