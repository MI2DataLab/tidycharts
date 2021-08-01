

#---
draw_x_axis_scatter <- function(shift_x, shift_y, space_size, width_of_one ,maximum, minimum){
  ticks <- ""
  tick <- 0
  while(tick <= maximum){
    ticks <- paste(
      ticks,
      #add new tick
      draw_line(80 + shift_x + width_of_one*tick, 80+width_of_one*tick + shift_x, 250, 251.6),
      add_label(80+width_of_one*tick+shift_x, 251.6+4.8+6 + 1.6, tick),
      #adding grid lines
      draw_line(80+width_of_one*tick+shift_x, 80+width_of_one*tick + shift_x, 250 + shift_y, 50-4.8, "black", 0.1),
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
      draw_line(80 + shift_x + width_of_one*tick, 80 + width_of_one*tick + shift_x, 250, 251.6),
      add_label(80 + width_of_one*tick+shift_x, 251.6+4.8+6 + 1.6, tick),
      #adding grid lines
      draw_line(80 + width_of_one*tick+shift_x, 80+width_of_one*tick + shift_x, 250 + shift_y, 50-4.8,  "black", 0.1),
      sep='\n'
    )
    tick <- tick - space_size
  }

  return(paste(
    #adding ticks
    ticks,
    #whole x-axis
    draw_line(80-4.8, 334.8 + shift_x, 250, 250),
    sep='\n'
  ))
}

#---
draw_y_axis <- function(shift_x, shift_y, space_size, height_of_one, maximum, minimum){
  ticks <- ""
  tick <- space_size
  while(tick <= maximum){
    ticks <- paste(
      ticks,
      draw_line(78.4 + shift_x, 80 + shift_x, 250 - height_of_one*tick, 250 - height_of_one*tick),
      add_label(80-1.6-4.8 + shift_x, 250 - height_of_one*tick +6, tick, anchor="end"),
      #adding grid lines
      draw_line(80, 330 + shift_x, 250 - height_of_one*tick, 250 - height_of_one*tick,"black", 0.1),
      sep='\n'
    )
    tick <- tick+space_size
  }
  tick <- -space_size
  while(tick >= minimum){
    ticks <- paste(
      ticks,
      draw_line(78.4 + shift_x, 80 + shift_x, 250 - height_of_one*tick, 250 - height_of_one*tick),
      add_label(80-1.6-4.8 + shift_x, 250 - height_of_one*tick +6, tick, anchor="end"),
      #adding grid lines
      draw_line(80, 330 + shift_x, 250 - height_of_one*tick, 250 - height_of_one*tick, "black", 0.1),
      sep='\n'
    )
    tick <- tick-space_size
  }
  return(paste(
    #adding ticks
    ticks,
    #whole y-axis
    draw_line(80 + shift_x, 80+shift_x, 50 - 4.8, 250 + shift_y + 4.8),
    sep='\n'
  ))
}

#---
add_scatter_legend <- function(shift_x, title, categories, colors){
  legend <- add_label(334.8+4.8 + shift_x, 56, title, anchor="start")
  y <- 50+4.8+12
  for(i in 1:length(categories)){
    legend <- paste(
      legend,
      draw_circle(343.6 + shift_x, y, colors[i]),
      add_label(343.6+4.8+2.4 + shift_x, y+6, categories[i], anchor="start"),
      sep='\n'
    )
    y <- y + 4.8+ 12
  }
  return(legend)
}

#---
draw_scatter_points <- function(svg_string, data, x, y,cat, x_space_size, y_space_size, x_names, y_names, legend_title, bubble_value){ #labels_vector - x axis name, y axis name
  points <- ""
  colors <- c("rgb(89,79,223)", "rgb(115,65,183)", "rgb(31, 210, 237)", "rgb(96,124,180)", "rgb(30,69,148)", "rgb(64,64,64)")
  categories <- unique(cat)
  maximum <- max(abs(y))
  x_maximum <-max(abs(x))
  height_of_one <- 200/maximum
  width_of_one <- 250/x_maximum
  #dealing with negative values
  neg_x <-x[x<0]
  neg_y <- y[y<0]
  #calculating the shifts
  shift_y <- height_of_one*abs(min(neg_y))
  if(is.finite(shift_y)==FALSE){shift_y <- 0} #in case there are no negative values
  shift_x <- width_of_one*abs(min(neg_x))
  if(is.finite(shift_x)==FALSE){shift_x <- 0} #in case there are no negative values

  if(is.null(bubble_value)== FALSE){
    bubble_min <- min(bubble_value)
    r_of_one <- 2.4/bubble_min
  }

  for (i in 1:length(x)){
    cat_index <- match(cat[i],categories)[1]
    color <- colors[cat_index]
    if(is.null(bubble_value)== FALSE){
      stop_if_bubble_negative(bubble_value)
      points <- paste(points, draw_circle(80 + width_of_one*x[i] + shift_x, 250 - height_of_one*y[i], color, r_of_one*bubble_value[i], opacity=0.55), sep='\n')
    }else{
      points <- paste(points, draw_circle(80 + width_of_one*x[i] + shift_x, 250 - height_of_one*y[i], color), sep='\n')
    }


  }
  return(paste(svg_string,
               draw_x_axis_scatter(shift_x, shift_y,x_space_size, width_of_one, max(x), min(x)),
               draw_y_axis(shift_x, shift_y,y_space_size, height_of_one, max(y), min(y)),
               add_label( 334.8+4.8 + shift_x, 250+6, x_names[1], anchor="start"),
               add_label( 334.8+4.8 + shift_x, 250+4.8+12, x_names[2], anchor="start"),
               add_label( 80-4.8 + shift_x, 50 - 4.8 - 6 - 6 -4.8, y_names[1], anchor="end"),
               add_label( 80-4.8 + shift_x, 50 -6 -4.8, y_names[2], anchor="end"),
               add_scatter_legend(shift_x, legend_title, categories, colors),
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
  maximum <- max(abs(y))
  x_maximum <-max(abs(x))
  height_of_one <- 200/maximum
  width_of_one <- 250/x_maximum

  #dealing with negative values
  neg_x <-x[x<0]
  neg_y <- y[y<0]
  #calculating the shifts
  shift_y <- height_of_one*abs(min(neg_y))
  if(is.finite(shift_y)==FALSE){shift_y <- 0} #in case there are no negative values
  shift_x <- width_of_one*abs(min(neg_x))
  if(is.finite(shift_x)==FALSE){shift_x <- 0} #in case there are no negative values


  initialize(width = 80 + shift_x + 250 + 80, height= 250 + shift_y + 20) %>%
    draw_scatter_points(.,data, x, y, cat, x_space_size, y_space_size, x_names, y_names, legend_title, bubble_value) %>%
    finalize() #%>% show()
}

#--- test ---
data <- data.frame(
  x = c(2, 3, 5, 5.5, 7, 9, 2.5, 1, 5, 5.3, 8.5, 6.6),
  value = c(5,3,2,6, 7, 3, 2, 1,7,8,3, 5),
  cat = c("mlem","mlem","mlem","mlem","mlem", "kwa","kwa","kwa", "moo","moo","moo","moo"),
  bubble =c (1,2,3,4,5,4,6,2,1,3, 3.5, 4.5 )
)

#scatter_plot(data, data$x, data$value, data$cat, 2, 1, c("time", "in s"), c("distance", "in km"), "Legenda", data$bubble) %>% SVGrenderer()
#p <- penguins %>%
#  drop_na(bill_length_mm, bill_depth_mm)

#scatter_plot(p, p$bill_length_mm, p$bill_depth_mm, p$species, 10, 5, c("bill length", "in mm"), c("bill depht", "in mm"), "Legend") %>%
#  add_title("Relationship between bill length and bill depth","","") %>%
#  SVGrenderer()

data("mtcars")
df <- mtcars
# Convert cyl as a grouping variable
df$cyl <- as.factor(df$cyl)
# Inspect the data
#head(df[, c("wt", "mpg", "cyl", "qsec")], 4)

#scatter_plot(df, df$wt,df$mpg, df$cyl, 1, 5, c("car's weigt", "in 1000lbs"), c("number of cylinders", ""), "Legend", df$hp) %>% SVGrenderer()

