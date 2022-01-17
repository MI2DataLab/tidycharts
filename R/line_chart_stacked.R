

#---
add_marker <- function(data, cat, value, x, height_of_one, k, y, show_label, cat_width){ #cat jest calym wektore, k to numer serii w ktorej jestesmy y - przesuniecie

  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  if(is.na(show_label) == FALSE){
    value_label <- add_label(x, top_margin+200-y - (height_of_one*value/2) +6, value, get_color_stacked(k)$text_color )
  }else{
    value_label<-""
  }
return(paste(
    #asisting line
    draw_line(x, x, top_margin, top_margin+200, "white", 0.1),
    #draw rectangle marker
    draw_rect(x-2.4, (top_margin+197.6-(height_of_one*value))-y, "rgb(127,127,127)", 4.8, 4.8),
    #label with value
    #add_label(x,250-y - (height_of_one*value/2) +6, value, "white" ),
    value_label,
    sep="\n"

  ))

}

#----
draw_polygons <- function(svg_string, data, cat, series, series_labels, show_labels, cat_width){ #show label - vector with null or not null

  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  polygons <- svg_string
  colors <- c("rgb(64,64,64)","rgb(166,166,166)","rgb(70,70,70)","rgb(90,90,90)" , "rgb(110,110,110)","rgb(127,127,127)" )
  x = left_margin
  labels <- ""

  all_sums <- rowSums(data[,series])
  height_of_one <- 200/max(all_sums)
  y <- rep(0, length(cat))
  #-----
  for(k in 1:(length(series))){ #points which series it is

    color <- get_color_stacked(k)$bar_color
    values <- data[, series[k]]

    #series labels
    labels <- paste(labels,
                    add_label(left_margin-4.8, top_margin+200-y[1] - (height_of_one*values[1]/2), series_labels[k], anchor="end"),
                    sep="\n"

    )

    for(i in 1:(length(cat)-1)){ #going through categories
      if (k == 1) {
        polygons <- paste(
          polygons,
          #adding ticks
          draw_line(x, x, top_margin+200, top_margin+201.6),
          #xaxis line
          draw_line(x - cat_width / 2, x + cat_width / 2, top_margin+200, top_margin+200),
          #category label
          add_label(x, top_margin+218.4, cat[i]),
          sep = '/n'
        )
      }
      polygons <- paste(polygons,
                        #draw area
                        draw_quadrangle(x, (top_margin+200-(height_of_one*values[i])) - y[i],
                                        x + cat_width, (top_margin+200-(height_of_one*values[i+1])) - y[i+1],
                                        x + cat_width, top_margin+200 - y[i+1],
                                        x, top_margin+200 - y[i],
                                        color),
                      add_marker(data, cat[i], values[i], x, height_of_one, k, y[i], show_labels[i], cat_width),
                      sep='\n')

      x <- x + cat_width
      y[i] <- y[i] + height_of_one*values[i]
    }
    j <- length(cat)
    if (k == 1) {
      polygons <- paste(
        polygons,
        #adding ticks
        draw_line(x, x, top_margin+200, top_margin+201.6),
        #xaxis line
        draw_line(x - cat_width / 2, x + cat_width / 2, top_margin+200, top_margin+200),
        #category label
        add_label(x, top_margin+218.4, cat[j]),
        sep = '/n'
      )
    }
    polygons <- paste(polygons,
                      add_marker(data, cat[j], values[j], x, height_of_one, j, y[j], show_labels[j], cat_width),
                      sep='\n')
    x <- left_margin
    y[j] <- y[j] + height_of_one*values[j]

  }
  x <- left_margin

  #sums labels
  for(i in 1:length(all_sums)){
   if(is.na(show_labels[i])==FALSE){
     labels <- paste(labels,
                     add_label(x, top_margin+200 - y[i] -4.8-6, all_sums[i]),
                     sep='\n')
   }
    x<-x + cat_width
  }

  return (paste(polygons, labels, sep='\n'))
}

#----
#' Generates areas (stacked lines) plot. If more than one series is supplied, stacked areas plot is generated.
#'
#' @param data data frame containing data to be plotted
#' @param cat vector containing time intervals of the values
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels vector containing names of series to be shown on the plot
#' @param show_labels vector of the same length as cat containing NA or not NA values defining which categories should have labels of values displayed
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
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
#' line_chart_stacked <- line_chart_stacked(data, data$weeks, series, series, labels, "weeks")
#'
#' #show the plot
#' line_chart_stacked
#'
line_chart_stacked <- function(data, cat, series, series_labels, show_labels, interval = "months"){

  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  cat_width <- get_interval_width(interval)$category_width
  svg_string <- initialize(width = left_margin + cat_width*length(cat) +80, height = 300) %>%
    draw_polygons(data, cat, series, series_labels, show_labels, cat_width) %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}

