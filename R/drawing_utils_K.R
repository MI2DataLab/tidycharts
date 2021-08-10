

draw_rect <- function(x, y, color, width, height, style = NULL){
  if(is.null(style)){
    fill <- color
    stroke <- color
  }
  else{
    styling <- get_style(style)
    fill <- styling[['fill']]
    stroke <- styling[['stroke']]
  }
  if (width < 0) {
    x <- x + width
    width <- -1 * width
  }
  if (height < 0) {
    y <- y + height
    height <- -1 * height
  }
  return(paste0('<rect x="',
                x,
                '" y="',
                y,
                '" width="',
                width,
                '" height="',
                height,
                '" fill="',
                fill,
                '" stroke="',
                stroke,
                '" />'))}


add_label<- function(x, y, value, color="black", anchor="middle"){
  if (length(value) < 1) {
    value = ''
  }
  parsed <- lapply(strsplit(as.character(value), '\n'), trimws)[[1]]
  y_offset <- 0
  svg_string <- character()
  for (line in parsed) {
    svg_string <- paste(svg_string,
                        paste0('<text x="',
                               x,
                               '" y="',
                               y + y_offset,
                               '"  font-family="Arial" font-size="12" text-anchor="',
                               anchor,
                               '" fill="',
                               color,
                               '" >',
                               line,
                               '</text>'),
                        sep = '\n')
    y_offset <- y_offset + 12
  }
  return(svg_string)
}


draw_line <- function(x1, x2, y1, y2, color="black", stroke_width=1.6){
  return(paste0('<line x1="',
                x1,
                '" x2="',
                x2,
                '" y1="',
                y1,
                '" y2="',
                y2,
                '" stroke="',
                color,
                '" stroke-width="',
                stroke_width,
                '"/>'
                ))
}

draw_quadrangle <- function(x1,y1,x2,y2,x3,y3,x4,y4, color){
  return(paste0( '<polygon points="',
                 x1,
                 ", ",
                 y1,
                 ' ',
                 x2,
                 ', ',
                 y2,
                 ' ',
                 x3,
                 ', ',
                 y3,
                 ' ',
                 x4,
                 ', ',
                 y4,
                 '" stroke="',
                 color,
                 '" fill="',
                 color,
                 '" />'

  ))
}


add_index <- function(x, y, text="100"){ #x defines how long the line is
  return(paste(
    paste0('<line x1="80" x2="',
           x,
           '" y1="',
           y - 0.8,
           '" y2="',
           y,
           '" stroke="rgb(64,64,64)" stroke-width="1.6"/>'),
    #drawing a triangle
    paste0(
      '<polygon points="',
      x,
      ', ',
      y,
      ' ',
      x+16,
      ', ',
      y+4.8,
      ' ',
      x+16,
      ', ',
      y-4.8,
      '" stroke="black" fill="black"/> '),
    paste0(
      '<text x="',
      x + 16 + 4.8,
      '" y="',
      y + 4.8,
      '" font-family="Arial" text-anchor="start" font-size="12" >',
      text,
      '</text>'),
    sep='\n'


  ))

}


draw_circle <- function(x,y, color, radius=2.4, opacity = 1){
  return(paste0('<circle cx="',
                x,
                '" cy="',
                y,
                '" r="',
                radius,
                '"  fill="',
                color,
                '" stroke="',
                color,
                '" fill-opacity="',
                opacity,
                '" />'))}

add_vertical_index <- function(x, y, text="100"){ #y defines how long the index line will be
  return(paste(
    paste0(
      '<polygon points="',
      x,
      ', 50 ',
      x+ 4.8,
      ', 34 ',
      x - 4.8,
      ', 34" stroke="black" fill="black"/> '
    ),
    paste0(
      '<text x="',
      x + 6.4,
      '" y="45" font-family="Arial" text-anchor="start" font-size="12" >',
      text,
      '</text>'
    ),
    paste0('<line x1="',
           x,
           '" x2="',
           x,
           '" y1="50" y2="',
           y,
           '" stroke="black" stroke-width="1.6"/>'),
    sep='\n'
  ))
}


find_height <- function(data, series){
  maxes <- c()
  for(k in 1:(length(series))){
    maxes <- c(maxes, max(data[,series[k]]))
  }
  maximum <- max(maxes)
  height_of_one <- 200/maximum
  return(height_of_one)
}

# normalize <- function(data, cat, series){
#   df <- data
#   all_sums <- rowSums(df[,series])
#   for(i in 1:length(cat)){
#     sum <- all_sums[i]
#     for(k in 1:length(series)){
#       df[,series[k]][i] <- df[,series[k]][i]/sum*100
#     }
#   }
#   return(df)
# }

