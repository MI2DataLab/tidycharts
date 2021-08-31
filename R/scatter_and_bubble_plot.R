
#---
draw_x_axis_scatter <- function(shift_x, shift_y, space_size, width_of_one ,x_end, minimum, x_start, x){
  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  ticks <- ""
  if(is.null(space_size)){
    pretty_vector <- pretty(x_start:x_end, 8)
    space_size <- pretty_vector[2] - pretty_vector[1]
  }
  if(x_start==0 && is.null(space_size)){x_start <- pretty_vector[1]}
  #if(x_end==max(x)){x_end <- pretty_vector[8]}
  tick <- x_start + space_size
  while(tick <= x_end){
    ticks <- paste(
      ticks,
      #add new tick
      draw_line(left_margin + shift_x + width_of_one*(tick - x_start), left_margin + width_of_one*(tick - x_start) + shift_x, top_margin + 200, top_margin + 201.6),
      add_label(left_margin+width_of_one*(tick - x_start)+shift_x, top_margin +201.6+4.8+6 + 1.6, format(tick, digits = 2)),
      #adding grid lines
      draw_line(left_margin+width_of_one*(tick - x_start)+shift_x, left_margin+width_of_one*(tick - x_start) + shift_x, top_margin + 200 + shift_y, top_margin-4.8, "black", 0.1),
      sep='\n'
    )
    tick <- tick+space_size
  }
  #drawing negative values
  tick <- -space_size
  while(tick >= minimum){
    ticks <- paste(
      ticks,
      #add new tick
      draw_line(left_margin + shift_x + width_of_one*tick, left_margin + width_of_one*tick + shift_x, 200+top_margin, 201.6+top_margin),
      add_label(left_margin + width_of_one*tick+shift_x, top_margin+201.6+4.8+6 + 1.6, tick),
      #adding grid lines
      draw_line(left_margin + width_of_one*tick+shift_x, left_margin+width_of_one*tick + shift_x, top_margin+200 + shift_y, top_margin-4.8,  "black", 0.1),
      sep='\n'
    )
    tick <- tick - space_size
  }

  return(paste(
    #adding ticks
    ticks,
    #whole x-axis
    draw_line(left_margin-4.8, left_margin+254.8 + shift_x,top_margin +200, 200 + top_margin),
    sep='\n'
  ))
}

#---
draw_y_axis <- function(shift_x, shift_y, space_size, height_of_one, y_end, minimum, y_start, y){
  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  ticks <- ""
  if(is.null(space_size)){
    pretty_vector <- pretty(y_start:y_end, 8)
    space_size <- pretty_vector[2] - pretty_vector[1]
  }
  if(y_start==0 && is.null(space_size)){y_start <- pretty_vector[1]}
  #if(y_end==max(y)){y_end <- pretty_vector[8]}

  tick <- space_size + y_start
  while(tick <= y_end){
    ticks <- paste(
      ticks,
      draw_line(left_margin-1.6 + shift_x, left_margin + shift_x, top_margin + 200 - height_of_one*(tick - y_start), top_margin +  200 - height_of_one*(tick - y_start)),
      add_label(left_margin-1.6-4.8 + shift_x,top_margin + 200 - height_of_one*(tick - y_start) + 6 , format(tick, digits = 2), anchor="end"),
      #adding grid lines
      draw_line(left_margin, left_margin+ 250 + shift_x, top_margin+200 - height_of_one*(tick - y_start), top_margin+200 - height_of_one*(tick - y_start),"black", 0.1),
      sep='\n'
    )
    tick <- tick + space_size
  }
  tick <- -space_size
  while(tick >= minimum){
    ticks <- paste(
      ticks,
      draw_line(left_margin-1.6 + shift_x, left_margin + shift_x, top_margin+200 - height_of_one*tick, top_margin+200 - height_of_one*tick),
      add_label(left_margin-1.6-4.8 + shift_x, top_margin+200 - height_of_one*tick +6, tick, anchor="end"),
      #adding grid lines
      draw_line(left_margin, left_margin+250 + shift_x,top_margin +200 - height_of_one*tick, top_margin+200 - height_of_one*tick, "black", 0.1),
      sep='\n'
    )
    tick <- tick-space_size
  }
  return(paste(
    #adding ticks
    ticks,
    #whole y-axis
    draw_line(left_margin + shift_x, left_margin + shift_x, top_margin - 4.8, top_margin+200 + shift_y + 4.8),
    sep='\n'
  ))
}

#---
add_scatter_legend <- function(shift_x, title, categories){
  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  legend <- add_label(left_margin+254.8+4.8 + shift_x, top_margin+6, title, anchor="start")
  y <- top_margin+4.8+12
  for(i in 1:length(categories)){
    legend <- paste(
      legend,
      draw_circle(left_margin+263.6 + shift_x, y, get_scatter_colors(i)),
      add_label(left_margin+263.6+4.8+2.4 + shift_x, y+6, categories[i], anchor="start"),
      sep='\n'
    )
    y <- y + 4.8+ 12
  }
  return(legend)
}

#---
draw_scatter_points <- function(svg_string, data, x, y, cat, x_space_size, y_space_size, x_names, y_names, legend_title, bubble_value, shift_y, shift_x, width_of_one, height_of_one, x_start, x_end, y_start, y_end){ #labels_vector - x axis name, y axis name
  left_margin <- get_margins()$left
  top_margin <- get_margins()$top
  points <- ""
  categories <- unique(cat)

  for (i in 1:length(x)){
    if(length(categories)==0){
      color <- get_scatter_colors(1)
    }else{
      cat_index <- match(cat[i],categories)[1]
      color <- get_scatter_colors(cat_index)
    }

    if(is.null(bubble_value)== FALSE){
      stop_if_bubble_negative(bubble_value)
      bubble_min <- min(bubble_value)
      r_of_one <- 2.4/sqrt(bubble_min)
      points <- paste(points, draw_circle(left_margin + width_of_one*(x[i] - x_start) + shift_x, top_margin+200 - height_of_one*(y[i] - y_start), color, r_of_one*bubble_value[i], opacity=0.55), sep='\n')
    }else{
      points <- paste(points, draw_circle(left_margin + width_of_one*(x[i] - x_start) + shift_x, top_margin+200 - height_of_one*(y[i] - y_start), color), sep='\n')
    }


  }
  svg_string <- paste(svg_string,
        draw_x_axis_scatter(shift_x, shift_y, x_space_size, width_of_one, x_end, min(x), x_start, x),
        draw_y_axis(shift_x, shift_y,y_space_size, height_of_one, y_end, min(y), y_start, y),
        add_label( left_margin+254.8 + 4.8 + shift_x, top_margin+200+6, x_names[1], anchor="start"),
        add_label(left_margin + 254.8 +4.8 + shift_x, top_margin+200 + 4.8+12, x_names[2], anchor="start"),
        add_label( left_margin-4.8 + shift_x, top_margin - 4.8 - 6 - 6 -4.8, y_names[1], anchor="end"),
        add_label( left_margin-4.8 + shift_x, top_margin -6 -4.8, y_names[2], anchor="end"),
        points,
        sep='\n')
  if(length(categories) > 1){
    svg_string <- paste(svg_string,
                        add_scatter_legend(shift_x, legend_title, categories),
                        sep = '\n')
    }

  return(svg_string)
}



#---
#' Generates a scatter plot. If additional argument added, a bubble plot is generated.
#'
#' @param data data frame containing data to be plotted
#' @param x string containing a column name or a vector containing x - coordinates of values
#' @param y string containing a column name or a vector containing y - coordinates of values
#' @param cat string containing a column name or a vector containing categories of the values
#' @param x_space_size,y_space_size numeric value of the space between the ticks on the x,y - axis. Defaultly, axis will be divided into 8 sections
#' @param x_names vector containing two values:
#' * name of the value presented on the x - axis
#' * units of values presented on the x - axis
#' @param y_names vector containing two values:
#' * name of the value presented on the y - axis
#' * units of values presented on the y - axis
#' @param legend_title title of the legend
#' @param bubble_value vector containing values defining the size of bubbles. Set by default to NULL.
#' @param x_start numeric value defining where the x axis should start at. Set by default to 0.
#' @param x_end numeric value defining where the x axis should end at. Set by default to max(x).
#' @param y_start numeric value defining where the y axis should start at. Set by default to 0.
#' @param y_end numeric value defining where the y axis should end at. Set by default to max(y).
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' # prepare a data frame
#' data <- data.frame(
#'     x = c(2, -3, -5, 5.5, 7, 9, 2.5, 1, 5, 5.3, 8.5, 6.6),
#'     value = c(5,-3,2,6, 7, 3, -2, 1,7,8,3, -5),
#'     cat = c("val1","val1","val2","val2","val2",
#'             "val3","val3","val3", "val4","val4","val4","val4"),
#'     bubble = c (1,2,12,4,5,4,8,2,1,9, 8, 4.5 )
#')
#'
#' # generate character vectors with svg data
#' scatter <- scatter_plot(
#'   data = data,
#'   x = data$x,
#'   y = data$value,
#'   cat = data$cat,
#'   x_space_size = 2,
#'   y_space_size = 1,
#'   x_names = c("time", "in s"),
#'   y_names = c("distance", "in km"),
#'   legend_title = "Legend")
#'
#' bubble <-scatter_plot(
#'   data = data,
#'   x = data$x,
#'   y = data$value,
#'   cat = data$cat,
#'   x_space_size = 2,
#'   y_space_size = 1,
#'   x_names = c("time", "in s"),
#'   y_names = c("distance", "in km"),
#'   legend_title = "Legend",
#'   bubble_value = data$bubble)
#'
#' # show the plots
#' scatter
#' bubble
#'
scatter_plot <-
  function(data,
           x,
           y,
           cat = NULL,
           #x_space_size = (x_end - x_start) / 8,
           #y_space_size = (y_end - y_start) / 8,
           x_space_size = NULL,
           y_space_size = NULL,
           x_names = c('x',''),
           y_names = c('y',''),
           legend_title="Legend",
           bubble_value = NULL,
           x_start = 0,
           x_end = max(get_vector(data, x)),
           y_start = 0,
           y_end = max(get_vector(data, y))) {
    height_of_one <- 200 / (y_end - y_start)
    width_of_one <- 250 / (x_end - x_start)
    left_margin <- get_margins()$left
    top_margin <- get_margins()$top


    if(length(x) == 1){x <- data[ , x]}
    if(length(y) == 1){y <- data[ , y]}
    if (is.null(bubble_value)==FALSE) {
      if(length(bubble_value) == 1){bubble_value <- data[ , bubble_value]}
    }
    if (is.null(cat)) {
      cat <- rep("", length(x))
    }else{
      if(length(cat) == 1){cat <- data[ , cat]}
    }

    #dealing with negative values
    neg_x <- x[x < 0]
    neg_y <- y[y < 0]
    #calculating the shifts
    if (length(neg_y) == 0) {
      shift_y <- 0
    }
    else{
      shift_y <- height_of_one * abs(min(neg_y))
    }
    if (length(neg_x) == 0) {
      shift_x <- 0
    }
    else{
      shift_x <- height_of_one * abs(min(neg_x))
    }
    svg_string <- initialize(width = left_margin + shift_x + 250 + 80,
               height = top_margin + 200 + shift_y + 20) %>%
      draw_scatter_points(
        data,
        x,
        y,
        cat,
        x_space_size,
        y_space_size,
        x_names,
        y_names,
        legend_title,
        bubble_value,
        shift_y,
        shift_x,
        width_of_one,
        height_of_one,
        x_start,
        x_end,
        y_start,
        y_end
      ) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }
