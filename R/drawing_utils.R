pkg.env <- new.env(parent = emptyenv())


pkg.env$styles_df <-
  rbind(
    actual = c("rgb(64,64,64)", "rgb(64,64,64)"),
    prevoius =
      c("rgb(166,166,166)", "rgb(166,166,166)"),
    forecast =
      c("url(#diagonalHatch)", "rgb(64,64,64)"),
    plan = c("white", "rgb(64,64,64)"),
    total_white = c("white", "white")
  )
colnames(pkg.env$styles_df) <- c("fill", "stroke")



pkg.env$colors_df <- cbind(
  bar_colors =  c(
    "rgb(64,64,64)",
    "rgb(166,166,166)",
    "rgb(70,70,70)",
    "rgb(90,90,90)" ,
    "rgb(110,110,110)",
    "rgb(127,127,127)"
  ),
  text_colors = c("white", "black", "white", "white", "white", "black")
)

draw_bar <- function(svg_string, x, y, height, width, color = "black", style = NULL) {
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


draw_text <- function(svg_string, text, x, y, font_size = 12, text_anchor = "middle", text_color = "black", text_weight = "") {
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
      '" font-weight="',
      text_weight,
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
  stopifnot(orientation %in% c('top', 'right', 'bottom', 'left'))
  transformation <- switch(orientation,
                           "left" = "rotate(0,",
                           "top" = "rotate(90,",
                           "right" = "rotate(180,",
                           "bottom" = "rotate(270,")
  transformation <- paste0(transformation, tip_position_x,",",tip_position_y,")")

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

get_style <- function(style, styles_df = pkg.env$styles_df){
  return(styles_df[style, ])
}

#' Function to get bar/area color for stacked plots.
#'
#' @param series_number what is the number of the series. one of 1:6.
#' @param colors_df data frame with variety of colors
#'
#' @return list with bar_color and text_color
#'
#' @examples
get_gray_color_stacked <- function(series_number, colors_df = pkg.env$colors_df){

  stopifnot(series_number %in% 1:6)
  return(list(bar_color = colors_df[series_number,][['bar_colors']],
              text_color = colors_df[series_number,][['text_colors']]))
}

#' Caclulate string width in pixels
#'
#' @param string string which width will be calculated
#' @param bold boolean value, if string will be written in bold
#'
#' @return string width in pixels
#' @export
#'
#' @examples
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
