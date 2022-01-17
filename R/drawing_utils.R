
draw_bar <- function(svg_string, x, y, height, width, color = "black", style = NULL, translate_vec = c(0,0)) {
  if(height < 0 ){
    y = y + height
    height = -1 * height
  }
  if(is.null(style)){
    fill <- color
    stroke <- color
  }
  else{
    styling <- get_style(style)
    fill <- styling[['fill']]
    stroke <- styling[['stroke']]
  }
  x <- x + translate_vec[1]
  y <- y + translate_vec[2]
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
      fill,
      '" stroke="',
      stroke,
      '" />'
    ),
    sep = "\n"
  )
  return(svg_string)
}


draw_text <- function(svg_string, text, x, y, font_size = 12, text_anchor = "middle", text_color = "black", text_weight = "", translate_vec = c(0,0)) {

  parsed <- lapply(strsplit(as.character(text), '\n'), trimws)[[1]]
  y_offset <- 0
  for (line in parsed){
    svg_string <- paste(
      svg_string,
      paste0(
        '<text x="',
        x + translate_vec[1],
        '" y="',
        y + translate_vec[2],
        '" font-size="',
        font_size,
        '"  font-family="Arial" text-anchor="',
        text_anchor,
        '" fill="',
        text_color,
        '" font-weight="',
        text_weight,
        '" transform="translate(0 ',
        y_offset,
        ')" >',
        line,
        '</text>'
      ),
      sep = "\n"
    )
    y_offset <- y_offset + font_size
  }
  return(svg_string)
}


draw_x_axis <- function(svg_string, x, y, bar_width, line_width = 1.6, translate_vec = c(0,0)) {
  svg_string <- paste(
    svg_string,
    paste0(
      '<rect x="',
      x + translate_vec[1],
      '" y="',
      y + translate_vec[2],
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


#' Draw triangle and append it to svg string
#'
#' @param svg_string svg string to paste a triangle
#' @param tip_position_x,tip_position_y  x, y position of tip of the triangle
#' @param style style of the triangle
#' @param translate_vec the translation vector
#' @param orientation where the triangle should be pointing. One of c('top', 'right', 'bottom', 'left').
#'
#' @return svg string
#'
draw_triangle <- function(svg_string, tip_position_x, tip_position_y, orientation = "left", style=NULL, translate_vec = c(0,0)){

  stopifnot(orientation %in% c('top', 'right', 'bottom', 'left'))
  transformation <- switch(orientation,
                           "left" = "rotate(0,",
                           "top" = "rotate(90,",
                           "right" = "rotate(180,",
                           "bottom" = "rotate(270,")
  transformation <- paste0(transformation, tip_position_x,",",tip_position_y,")")
  tip_position_x <-  tip_position_x + translate_vec[1]
  tip_position_y <-  tip_position_y + translate_vec[2]
  if(is.null(style)){
    fill <- ""
    stroke <- ""
  }
  else{
    styling <- get_style(style)
    fill <- styling[['fill']]
    stroke <- styling[['stroke']]
  }

  svg_string <- paste(svg_string,
                      # add white triangle in the background
                      paste0(
                        '<polygon points="',
                        tip_position_x-3,
                        ', ',
                        tip_position_y,
                        ' ',
                        tip_position_x + 13,
                        ', ',
                        tip_position_y - 7,
                        ' ',
                        tip_position_x + 13,
                        ', ',
                        tip_position_y + 7,
                        '" transform="',
                        transformation,
                        '" fill="white" stroke="white" />'
                      ),
                      # add styled triangle in the foreground
                      paste0(
                        '<polygon points="',
                        tip_position_x,
                        ', ',
                        tip_position_y,
                        ' ',
                        tip_position_x + 10,
                        ', ',
                        tip_position_y - 4,
                        ' ',
                        tip_position_x + 10,
                        ', ',
                        tip_position_y + 4,
                        '" transform="',
                        transformation,
                        '" fill="',
                        fill,
                        '" stroke="',
                        stroke,
                        '" />'
                      ),
                      sep = "\n")
  return(svg_string)
}


draw_ref_line_horizontal <- function(svg_string, x, bar_width, line_y, label) {
  left_margin <- get_margins()$left
  len <- bar_width * 1.5 * length(x)
  svg_string <- draw_bar(svg_string,
                         x = left_margin,
                         y = line_y,
                         width = len,
                         height = 1.6,
                         color = "black")
  svg_string <- draw_triangle(svg_string,
                              tip_position_x = len + left_margin,
                              tip_position_y = line_y + 1)
  svg_string <- draw_text(svg_string,
                          text = label,
                          x = len + 8 + 4.8 + left_margin,
                          y = line_y + 4,
                          text_anchor = "start")
  return(svg_string)
}

#' Calculate string width in pixels
#'
#' @param string string which width will be calculated
#' @param bold boolean value, if string will be written in bold
#'
#' @return string width in pixels
#'
str_width <- function(string, bold = FALSE){
  font <- ifelse(bold, 2, 1)
  graphics::strwidth(string, units = "inches", font = font, cex = 0.75) %>%
    inch2px() %>%
    return()
}


inch2px <- function(inches) inches * 96


normalize_rows <- function(df, x, series) {
  new_df <- data.frame(df)
  new_df[series] <- new_df[series] / rowSums(new_df[series]) * 100
  return(new_df)
}


#' Helper function to get the vector or column form df. If vector is passed it returns it.
#' If name of column is passed, it returns the column as a vector.
#'
#' @param df data frame with a column
#' @param vec name of the column in df or vector of values
#'
#' @return vector
#'
get_vector <- function(df, vec){
  if(length(vec) == 1) df[[vec]]
  else vec
}
