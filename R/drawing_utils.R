

draw_bar <- function(svg_string, x, y, height, width, color) {
  if(height < 0 ){
    y = y + height
    height = -1 * height
  }
  svg_string <- paste(
    svg_string,
    paste0(
      '<rect x="',
      x,
      '" y="',
      y,
      '" width="',
      width ,
      '" height="',
      height,
      '" fill="',
      color,
      '" />'
    ),
    sep = "\n"
  )
  return(svg_string)
}


draw_text <- function(svg_string, text, x, y, font_size = 12, text_anchor = "middle", text_color = "black") {
  svg_string <- paste(
    svg_string,
    paste0(
      '<text x="',
      x,
      '" y="',
      y,
      '" font-size="',
      font_size,
      '"  font-family="Arial" text-anchor="',
      text_anchor,
      '" fill="',
      text_color,
      '" >',
      text,
      '</text>'
    ),
    sep = "\n"
  )
  return(svg_string)
}


draw_x_axis <- function(svg_string, x, y, bar_width, line_width = 1.6) {
  svg_string <- paste(
    svg_string,
    paste0(
      '<rect x="',
      x,
      '" y="',
      y,
      '" width="',
      bar_width * 1.5,
      '" height="',
      line_width,
      '" fill="black" />'
    ),
    sep = '\n'
  )
  return(svg_string)
}


choose_waterfall_color <- function(height, pos_color, neg_color){
  if(height >= 0) pos_color
  else neg_color
}


choose_variance_colors <- function(colors){
  color <- list()
  if (colors == 1){
    color["pos_color"] <- "rgb(140,180,0)" # green
    color["neg_color"] <- "rgb(255,0,0)" # red
  }
  if (colors == 2){
    color["pos_color"] <- "rgb(255,0,0)" # red
    color["neg_color"] <- "rgb(140,180,0)" # green
  }
  return(color)
}


draw_triangle <- function(svg_string, tip_position_x, tip_position_y, orientation = "left", style=NULL){
  #' @param orientation where the triangle should be pointing. One of c('top', 'right', 'bottom', 'left').
  #' 
  # TODO add styling
  stopifnot(orientation %in% c('top', 'right', 'bottom', 'left'))
  transformation <- switch(orientation, 
                           "left" = "rotate(0,",
                           "top" = "rotate(90,",
                           "right" = "rotate(180,",
                           "bottom" = "rotate(270,")
  transformation <- paste0(transformation, tip_position_x,",",tip_position_y,")")
  svg_string <- paste(svg_string,
                      paste0(
                        '<polygon points="',
                        tip_position_x,
                        ', ',
                        tip_position_y,
                        ' ',
                        tip_position_x + 8,
                        ', ',
                        tip_position_y - 3,
                        ' ',
                        tip_position_x + 8,
                        ', ',
                        tip_position_y + 3,
                        '" transform="',
                        transformation,
                        '" />'
                      ),
                      sep = "\n")
  return(svg_string)
}


draw_ref_line_horizontal <- function(svg_string, x, bar_width, line_y, label) {
  len <- bar_width * 1.5 * length(x)
  svg_string <- draw_bar(svg_string,
                         x = 0,
                         y = line_y,
                         width = len,
                         height = 1.6,
                         color = "black")
  svg_string <- draw_triangle(svg_string,
                              tip_position_x = len,
                              tip_position_y = line_y + 1)
  svg_string <- draw_text(svg_string,
                          text = label,
                          x = len + 8 + 4.8,
                          y = line_y + 4,
                          text_anchor = "start")
  return(svg_string)
}

