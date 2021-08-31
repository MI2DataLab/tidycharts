
#podaje sie liste dataframow, wektor x kolejno odpowiednio, wektor y i cat

#data frame jest taki że s kolumny:
#x - procentowo w obrebie jednej kategorii gdzie ma być w punkt
#y - normlanie wartosc
#cat - categorie, z czego tu sie powatarzaja
#---
#musi byc osobny data frame dla kazdej serii

add_category_complex <- function(shift, data, cat, x, k, interval){ #cat jest calym wektorem
  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  cat_width <- get_interval_width(interval)$category_width
  return(paste(
    #linia
    draw_line(x - cat_width/2, x + cat_width/2, top_margin+200, top_margin+200),
    #label with the value
    add_label(x, top_margin+218.4 + shift, cat[k]),
    #asisting line
    draw_line(x - cat_width/2, x - cat_width/2, top_margin, top_margin+200 + shift, "black", 0.1),
    sep="\n"
  ))
}

#----
draw_lines_complex <- function(svg_string, list, vector_x, vector_y, vector_cat, series_labels, interval,df_numbers, point_cords){ #x,y,cat to string z nazwa kolumny
  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  maxes<-c()
  neg<-c()
  labels<-""
  cat_width <- get_interval_width(interval)$category_width

  for(k in 1:length(list)){
    maxes <- c(maxes, max(abs(list[[k]][,vector_y[k]])))
    neg <- c(neg,list[[k]][,vector_y[k]][list[[k]][,vector_y[k]]<0] )
  }
  height_of_one <- 200/max(maxes)
  #calculating the shift
  if(length(neg) == 0){shift <- 0}
  else{shift <- height_of_one*abs(min(neg))}


  lines<-""
  x_start <- left_margin
  x_to_connect <- left_margin
  for(k in 1:length(list)) {
    #---
    data <- list[[k]]
    x <- vector_x[k]
    y <- vector_y[k]
    cat <- vector_cat[k]
    #color <- colors[k]
    color <- get_color_stacked(k)$bar_color
    #labelka z nazwa serii
    labels <- paste(labels,
                    add_label(left_margin-4.8, top_margin+200- height_of_one*data[,y][1] + 6, series_labels[k], anchor="end"),
                    sep='\n')
    #---
    x_start <- left_margin
    categories <- unique(data[cat])
    for(i in 1:length(categories[,cat])){ #going through categories/ time series
      category <- categories[,cat][i]
      filtered <- data[data[cat] == category,]
      #lines between two categories
      if( i!= 1){
        lines <- paste(
          lines,
          draw_line(x_to_connect, x_start + cat_width*filtered[,x][1]/100, y_to_connect, top_margin+200-(height_of_one*filtered[,y][1]), color),
          sep='\n')
      }
      for(j in 1:(length(filtered[,x])-1)){ #going through points in one category/ time series
        lines <- paste(lines,
                       #linia
                       draw_line(x_start + cat_width*filtered[,x][j]/100, x_start + cat_width*filtered[,x][j+1]/100, top_margin+200-(height_of_one*filtered[,y][j]), top_margin+200-(height_of_one*filtered[,y][j+1]), color),
                       sep='\n')
      }
      lines <- paste(lines, add_category_complex(shift ,data, categories[,cat], x_start + cat_width/2, i, interval),sep='\n')
      x_to_connect <- x_start + cat_width*filtered[,x][j+1]/100
      x_start <- x_start + cat_width
      y_to_connect <- top_margin+200-(height_of_one*filtered[,y][j+1])
    }
  }
  #adding last assisting line
  lines <- paste(lines,
                 draw_line(left_margin + cat_width*length(categories[,cat]), left_margin + cat_width*length(categories[,cat]), top_margin, top_margin+200+shift, "black", 0.1),
                 sep='\n')
  chosen_points <- draw_chosen_points_complex(list, vector_x, vector_y, vector_cat, interval, df_numbers, height_of_one, point_cords, categories)
  return(paste(svg_string, lines, labels, chosen_points ,sep='\n'))
}

#---
draw_chosen_points_complex <- function(list, vector_x, vector_y, vector_cat, interval,df_numbers, height_of_one, point_cords,categories){
  chosen_points <- ""
  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  cat_width <- get_interval_width(interval)$category_width

  if(is.null(df_numbers)==FALSE){
  for(i in 1:length(df_numbers)){ #going through all chosen points
    k <- df_numbers[i] #index of data frame on the list
    data <- list[[k]]
    x <- vector_x[k]
    y <- vector_y[k]
    cat <- vector_cat[k]
    #color <- colors[k]
    color <- get_color_stacked(k)$bar_color

    #calculating x coordinate
    p_cat <- data[,cat][point_cords[i]]
    cat_index <- match(p_cat, categories[,vector_cat[length(list)]])[1] #which category it is
    x_start <- left_margin + cat_width*(cat_index-1)
    x_cir <- x_start + cat_width*data[,x][point_cords[i]]/100

    y_cir <- top_margin+200 - height_of_one*data[, y][point_cords[i]]
    circle_color <- color
    chosen_points <- paste(chosen_points,
                           draw_circle(x_cir, y_cir, circle_color, 2.4),
                           #label
                           add_label(x_cir, y_cir - 4.8 - 2.4, data[,y][point_cords[i]], "black"),
                           sep='\n')
  }}
  return(chosen_points)
}


#----
#' More customizable version of `line_chart_dense`. User can choose the points to highlight.
#'
#' @param list list of data frames, each representing one series. Data frame should consist of columns:
#' * containing numeric values from 0 to 100 defining the percentage of distance in one time interval of the point (x - coordinates of the point)
#' * containing the value of a point  (y - coordinates of the point)
#' * containing the time interval of the value
#' @param vector_x vector containing the names of columns with x - coordinates of the point in the data frames
#' @param vector_y vector containing the names of columns with y - coordinates of the point in the data frames
#' @param vector_cat vector containing the names of columns with time interval of the point in the data frames
#' @param series_labels vector containing names of series to be shown on the plot
#' @param df_numbers vector containing index of data frame in the list of a value to be marked
#' @param point_cords vector of the same length as df_numbers containing numerical values of indexes in data frame of values to be marked
#' @inheritParams column_chart
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#'
#' #preparing data frames
#' data <- data.frame(
#' xdata = c(1, 60,90, 30, 60, 90, 30, 60, 90, 45,95,45, 95),
#' ydata = c(5, -10, -15, 11, 16, 18, 25, 22, 18, 10, 8, 23, 28),
#' catdata = c("Jan","Jan", "Jan", "Feb","Feb", "Feb", "Mar",
#' "Mar", "Mar", "Apr", "Apr", "May", "May")
#' )
#'
#' df <- data.frame(
#'   xdf = c(1,60,90, 30, 60, 90, 30, 60, 90, 45,95,45, 95),
#'  ydf = c(25, 22,20, 18, 28, 35,33, 29, 30, 38,31,26, 22),
#'  catdf = c("Jan","Jan", "Jan", "Feb","Feb", "Feb", "Mar",
#'   "Mar", "Mar", "Apr", "Apr", "May", "May")
#')
#'
#' #defining the rest of the arguments
#' list <- list(data, df)
#' vector_x <- c("xdata", "xdf")
#' vector_y <- c("ydata", "ydf")
#' vector_cat <-c("catdata", "catdf")
#' df_numbers <- c(1,2,2, 1)
#' point_cords <- c(1, 3, 4, 10)
#'
#' #generating the svg string
#' plot<- line_chart_dense_custom(
#'   list,
#'   vector_x = c("xdata", "xdf"),
#'   vector_y = c("ydata", "ydf"),
#'   vector_cat = c("catdata", "catdf"),
#'   series_labels = c("Gamma inc.", "Delta inc."),
#'   df_numbers = df_numbers,
#'   point_cords = point_cords)
#'
#' #showing the plot
#' plot
#'
#'
line_chart_dense_custom <-
  function(list,
           vector_x,
           vector_y,
           vector_cat,
           series_labels,
           df_numbers = NULL,
           point_cords = NULL,
           interval = "months") {
    svg_string <- initialize() %>%
      draw_lines_complex(
        list,
        vector_x,
        vector_y,
        vector_cat,
        series_labels,
        interval,
        df_numbers,
        point_cords
      ) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }

#' Line chart with more points then categories on x-axis.
#'
#'
#' @param data Data frame in wide format.
#' @param dates Name of column in `data` which contains dates or vector of dates.
#' @param series Vector of column names in `data` with values of time series.
#' @param interval Granularity of x axis. One of c('weeks', 'months', 'quarters', 'years'). Default value is 'months'.
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'  x = seq.Date(as.Date('2021-01-01'), as.Date('2021-07-01'), length.out = 200),
#'  'Company_sin' = 5 * sin(seq(
#'    from = 0,
#'    to = 2 * pi,
#'    length.out = 200
#'    )) +  rnorm(200, mean = 5, sd = 0.5),
#'  'Company_cos' = 5 * cos(seq(
#'    from = 0,
#'    to = 2 * pi,
#'    length.out = 200
#'  )) +  rnorm(200, mean = 5, sd = 0.5))
#'
#' df <- head(df, n = 199)
#'
#' line_chart_dense(
#'   df,
#'   dates = 'x',
#'   series = c('Company_sin', 'Company_cos'))
#'
line_chart_dense <- function(data, dates, series, interval = 'months'){
  stopifnot(interval %in% c('weeks', 'months', 'quarters', 'years'))

  parse_time_series(data, dates, series, interval) %>%
    line_chart_dense_custom(
      vector_x = rep('x', length(series)),
      vector_y = rep('y', length(series)),
      vector_cat = rep('cat', length(series)),
      series_labels = series,
      df_numbers = 1,
      point_cords = NULL,
      interval = interval
    )
}
