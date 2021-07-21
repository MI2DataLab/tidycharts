
#podaje sie liste dataframow, wektor x kolejno odpowiednio, wektor y i cat

#data frame jest taki że s kolumny:
#x - procentowo w obrebie jednej kategorii gdzie ma być w punkt
#y - normlanie wartosc
#cat - categorie, z czego tu sie powatarzaja
#---
#musi byc osobny data frame dla kazdej serii

add_category_complex <- function(shift, data, cat, x,k){ #cat jest calym wektorem
  return(paste(
    #linia
    draw_line(x-24, x+24, 250, 250),
    #label with the value
    add_label(x, 268.4+shift, cat[k]),
    #asisting line
    draw_line(x-24, x-24, 50, 250+shift, "white", 0.1),
    sep="\n"
  ))
}

#----
draw_lines_complex <-function(svg_string, list, vector_x, vector_y, vector_cat, series_labels,df_numbers, point_cords){ #x,y,cat to string z nazwa kolumny
  maxes<-c()
  neg<-c()
  labels<-""
  colors <- c("rgb(64,64,64)","rgb(166,166,166)","rgb(70,70,70)","rgb(90,90,90)" , "rgb(110,110,110)","rgb(127,127,127)" )

  for(k in 1:length(list)){
    maxes <- c(maxes, max(abs(list[[k]][,vector_y[k]])))
    neg<- c(neg,list[[k]][,vector_y[k]][list[[k]][,vector_y[k]]<0] )
  }
  height_of_one <- 200/max(maxes)
  #calculating the shift
  shift <- height_of_one*abs(min(neg))
  if(is.finite(shift)==FALSE){shift <- 0} #in case there are no negative values

  lines<-""
  x_start <-80
  for(k in 1:length(list)) {
    #---
    data <- list[[k]]
    x <- vector_x[k]
    y <- vector_y[k]
    cat <- vector_cat[k]
    color <- colors[k]
    #labelka z nazwa serii
    labels <- paste(labels,
                    add_label(75.2, 250- height_of_one*data[,y][1] + 6, series_labels[k], anchor="end"),
                    sep='\n')
    #---
    x_start <-80
    categories <- unique(data[cat])
    for(i in 1:length(categories[,cat])){ #going through categories/ time series
      category <- categories[,cat][i]
      filtered <- data %>% dplyr::filter(cat==category)
      #lines between two categories
      if( i!=1){
        lines <- paste(
          lines,
          draw_line(x_to_connect,x_start + 48*filtered[,x][1]/100, y_to_connect, 250-(height_of_one*filtered[,y][1]), color),
          sep='\n')
      }
      for(j in 1:(length(filtered[,x])-1)){ #going through points in one category/ time series
        lines <- paste(lines,
                       #linia
                       draw_line(x_start + 48*filtered[,x][j]/100, x_start + 48*filtered[,x][j+1]/100, 250-(height_of_one*filtered[,y][j]), 250-(height_of_one*filtered[,y][j+1]), color),
                       sep='\n')
      }
      lines <- paste(lines, add_category_complex(shift ,data, categories[,cat], x_start+24, i),sep='\n')
      x_to_connect <- x_start + 48*filtered[,x][j+1]/100
      x_start <- x_start+48
      y_to_connect <- 250-(height_of_one*filtered[,y][j+1])
    }
  }
  #adding last assisting line
  lines <- paste(lines,
                 draw_line(80+48*length(categories[,cat]), 80+48*length(categories[,cat]), 50, 250+shift, "white", 0.1),
                 sep='\n')
  chosen_points <- draw_chosen_points_complex(list, vector_x, vector_y, vector_cat, df_numbers, height_of_one, point_cords, colors, categories)
  return(paste(svg_string, lines, labels, chosen_points, sep='\n'))
}

#---
draw_chosen_points_complex <- function(list, vector_x, vector_y, vector_cat, df_numbers, height_of_one, point_cords, colors, categories){
  chosen_points <- ""
  for(i in 1:length(df_numbers)){ #going through all chosen points
    k <- df_numbers[i] #index of data frame on the list
    data <- list[[k]]
    x <- vector_x[k]
    y <- vector_y[k]
    cat <- vector_cat[k]
    color <- colors[k]

    #calculating x coordinate
    p_cat <- data[,cat][point_cords[i]]
    cat_index <- match( p_cat, categories[,cat])[1] #which category it is
    x_start <- 80 + 48*(cat_index-1)
    x_cir <- x_start + 48*data[,x][point_cords[i]]/100

    y_cir <- 250 - height_of_one*data[, y][point_cords[i]]
    circle_color <- colors[k]
    chosen_points <- paste(chosen_points,
                           draw_circle(x_cir, y_cir, circle_color, 2.4),
                           #label
                           add_label(x_cir, y_cir - 4.8 - 2.4, data[,y][point_cords[i]], "black"),
                           sep='\n')
  }
  return(chosen_points)
}


#----
#' Generates a line plot with markers on chosen points, many points within one time interval allowed.
#'
#' @param list list of data frames, each representing one series. Data frame should consist of columns:
#' * containing numeric values from 0 to 100 defining the percentage of distance in one time interval of the point (x - coordinattes of the point)
#' * containing the value of a point  (y - coordinates of the point)
#' * containing the time interval of the value
#' @param vector_x vector containing the names of columns with x - coordinates of the point in the data frames
#' @param vector_y vector containing the names of columns with y - coordinates of the point in the data frames
#' @param vector_cat vector containing the names of columns with time interval of the point in the data frames
#' @param series_labels vector containing names of series to be shown on the plot
#' @param df_numbers vector containing index of data frame in the list of a value to be marked
#' @param point_cords vector of the same length as df_numbers containing numerical values of indexes in data frame of values to be marked
#'
#' @return SVG string containing chart
#' @export
#'
#' @examples
line_plot_many_points_complex <- function(list, vector_x, vector_y, vector_cat, series_labels,df_numbers, point_cords){
  initialize() %>%
    draw_lines_complex(.,list, vector_x, vector_y, vector_cat, series_labels,df_numbers, point_cords) %>%
    finalize() #%>% show()
}
