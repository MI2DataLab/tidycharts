

#---
draw_x_axis_scatter <- function(space_size, width_of_one ,maximum){
  ticks <- ""
  tick <- space_size
  while(tick < maximum){
    ticks <- paste(
      ticks,
      #add new tick
      draw_line(80+width_of_one*tick, 80+width_of_one*tick, 250, 251.6),
      add_label(80+width_of_one*tick, 251.6+4.8+6, tick),
      #adding grid lines
      draw_line(80+width_of_one*tick, 80+width_of_one*tick, 250, 50-4.8, "white", 0.1),
      sep='\n'
    )
    tick <- tick+space_size
  }
  return(paste(
    #adding ticks
    ticks,
    #whole x-axis
    draw_line(80, 334.8, 250, 250),
    sep='\n'
  ))
}

#---
draw_y_axis <- function(space_size, height_of_one, maximum){ #na przyklad 5
  ticks <- ""
  tick <- space_size
  while(tick < maximum){
    ticks <- paste(
      ticks,
      draw_line(78.4, 80, 250 - height_of_one*tick, 250 - height_of_one*tick),
      add_label(80-1.6-4.8,250 - height_of_one*tick , tick, anchor="end"),
      #adding grid lines
      draw_line(80, 330,250 - height_of_one*tick, 250 - height_of_one*tick,  "white", 0.1),
      sep='\n'
    )
    tick <- tick+space_size
  }
  return(paste(
    #adding ticks
    ticks,
    #whole x-axis
    draw_line(80, 80, 50-4.8+6, 250),
    sep='\n'
  ))
}

#---
add_scatter_legend <- function(title, categories, colors){
  legend <- add_label(334.8+4.8, 56, title, anchor="start")
  y <- 56+4.8+6
  for(i in 1:length(categories)){
    legend <- paste(
      legend,
      draw_circle(343.6, y, colors[i]),
      add_label(343.6+4.8+2.4, y+3, categories[i], anchor="start"),
      sep='\n'
    )
    y <- y+4.8+6
  }
  return(legend)
}

#---
draw_scatter_points <- function(svg_string, data, x, y,cat, x_space_size, y_space_size, x_names, y_names, legend_title, bubble_value){ #labels_vector - x axis name, y axis name
  points <- ""
  colors <- c("rgb(0,90,255)", "rgb(0,132,255)", "rgb(0,168,255)", "rgb(96,124,180)", "rgb(30,69,148)", "rgb(64,64,64)")
  categories <- unique(cat)
  maximum <- max(y)
  x_maximum <-max(x)
  height_of_one <- 200/maximum
  width_of_one <- 250/x_maximum
  if(is.null(bubble_value)== FALSE){
    bubble_min <- min(bubble_value)
    r_of_one <- 2.4/bubble_min
  }

  for (i in 1:length(x)){
    cat_index <- match(cat[i],categories)[1]
    color <- colors[cat_index]
    if(is.null(bubble_value)== FALSE){
      points <- paste(points, draw_circle(80 + width_of_one*x[i], 250 - height_of_one*y[i], color, r_of_one*bubble_value[i]), sep='\n')
    }else{
      points <- paste(points, draw_circle(80 + width_of_one*x[i], 250 - height_of_one*y[i], color), sep='\n')
    }


  }
  return(paste(svg_string,
               draw_x_axis_scatter(x_space_size, width_of_one, x_maximum),
               draw_y_axis(y_space_size, height_of_one, maximum),
               add_label( 334.8+4.8, 250+6, x_names[1], anchor="start"),
               add_label( 334.8+4.8, 250+4.8+12, x_names[2], anchor="start"),
               add_label( 80-4.8, 50, y_names[1], anchor="end"),
               add_label( 80-4.8, 50+4.8+6, y_names[2], anchor="end"),
               add_scatter_legend(legend_title, categories, colors),
               points,
               sep='\n'))
}



#---
#' Generates a scatter plot. If additional argument added, a bubble plot is generated.
#'
#' @param data data frame containing data to be plotted
#' @param x vector containing x - coordinates of values
#' @param y vector containing y - coordinates of values
#' @param cat vector cointaining categories of the values
#' @param x_space_size numeric value of the space between the ticks on the x- axis
#' @param y_space_size numeric value of the space between the ticks on the y- axis
#' @param x_names vector containing two values:
#' * name of the value presented on the x - axis
#' * units of values presented on the x - axis
#' @param y_names vector containing two values:
#' * name of the value presented on the y - axis
#' * units of values presented on the y - axis
#' @param legend_title title of the legend
#' @param bubble_value vector containing values defining the size of bubbles. Set by default to NULL.
#'
#' @returnSVG SVG string containing chart
#' @export
#'
#' @examples
scatter_plot <- function(data, x, y, cat, x_space_size, y_space_size, x_names, y_names, legend_title, bubble_value=NULL){
  initialize() %>%
    draw_scatter_points(.,data, x, y, cat, x_space_size, y_space_size, x_names, y_names, legend_title, bubble_value) %>%
    finalize() #%>% show()
}
