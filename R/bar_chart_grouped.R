
#---
add_bar_grouped <-
  function(shift,
           data,
           cat,
           series,
           k ,
           y,
           width_of_one,
           series_labels = NULL,
           df_styles = NULL) {
    #k - points to the row in data frame
    #df_with_real_values - to show real values after the normalization

    svg_string <- ""
    labels <- ""
    value_label <- ""
    x <- get_margins()$left + shift
    for (i in length(series):1){ #going through series
      value <- data[,series[i]] #a vector
      color <- get_color_stacked(i)
      if(length(series) == 2 || i == 1){
        styles <- df_styles[,length(series)-i+1]
      }else{
        styles <- df_styles[,i - 1]
      }


      if(i != 1 && value[k] < 0){
          x <- x - (width_of_one*abs(value[k]))
      }
      #first element in series defines the triangle markers
      if(i == 1){
        rect <- draw_triangle("", width_of_one*value[k] + x ,y+8 , orientation = "bottom", style=styles[k])
      }else{
        rect <- draw_rect(x, y - 4.8*(i-2), color$bar_color, (width_of_one*abs(value[k])), 16, style = styles[k])
      }
      x <- get_margins()$left + shift

      svg_string <- paste(svg_string, rect, labels,  sep = '\n')

      if(i == 2){
        if(value[k]>0){value_label <- paste(value_label,
                                            add_label((x + width_of_one*value[k]+4.8), y+12, value[k], anchor="start"),
                                            sep='\n')}
        else{value_label <- add_label(x + 4.8, y+12, value[k], anchor="start")}
      }

    }

    return(paste(svg_string,
                 # value label
                 #add_label((x+4.8), y+12,all_sums[k], anchor="start"),
                 value_label,
                 #category label
                 add_label( get_margins()$left - 7.8, y+14, cat[k], anchor="end"),
                 #vertical axis
                 draw_line(get_margins()$left + shift, get_margins()$left+shift, (y-4.8) - 4.8, (y+16+4.8)),
                 #labels,
                 sep = '\n'
    ))
  }

#---
draw_bars_grouped <- function(svg_string, data, cat, foreground, background, markers, series_labels, df_with_real_values=NULL, df_styles = NULL){
  bars <- svg_string
  y = get_margins()$top
  #series <- c(markers, foreground, background)
  if(length(foreground) == 1){forg <- data[ ,foreground]
  }else{
    forg <- foreground
    foreground <- "foreground"}
  if(length(background) == 1){backg <- data[ ,background]
  }else{
    backg <- background
    background <- "background"}
  if(length(markers) == 1){mark <- data[ ,markers]
  }else{
    mark <- markers
    markers <- "markers"}
  if(length(cat) == 1){cat <- data[ ,cat]
  }
  series <- c(markers, foreground, background)

  data <- cbind(mark, forg, backg)
  colnames(data) <- series
  data <- as.data.frame(data)
  neg <- data[, series][data[,series] < 0]
  #looking for the maximum value

  #for(k in 1:(length(series))){
  #  maxes <- c(maxes, max(abs(data[,series[k]])))
  #}
  #maximum <- max(maxes)
  maximum <- max(abs(data[,series]))
  width_of_one <- 200/maximum

  #dealing with negative values
  if(length(neg) == 0){shift <- 0
  }else{
    shift <- width_of_one*abs(min(neg))}

  #adding series labels
  #zakladamy sie że w series sa dwa albo trzy elementy
  if(length(series)==3){
    bars <- paste(
      bars,
      add_label(get_margins()$left + shift + data[,series[3]][1]*width_of_one/2, get_margins()$top - 4.8*(length(series)-1), series_labels[3]),
      sep= '\n'
    )
  }

  bars <- paste(bars,
                add_label(get_margins()$left + shift + data[,series[2]][length(cat)]*width_of_one/2, get_margins()$top + 24 * length(cat) + 4.8, series_labels[2]),
                add_bar_grouped(shift, data, cat, series, 1, y, width_of_one, series_labels, df_styles=df_styles),
                sep='\n')
  y <- y + 24
  for(i in 2:length(cat)){
    bars <- paste(bars,
                  add_bar_grouped(shift ,data, cat, series,i, y, width_of_one, df_styles = df_styles),
                  sep='\n')
    y <- y + 24
  }
  return (bars)
}


#' Generates grouped horizontal barchart with scenario triangles.
#'
#' @param cat vector containing category names of values
#' @inheritParams column_chart_grouped
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#'
#' #preparing data frames
#' data <- data.frame(
#' city = c("Berlin", "Paris", "London", "Munich", "Vienna"),
#' AC = c(592, 1166, 618, 795, 538),
#' PL = c(570, 950, 800, 780, 460),
#' triangles = c(545, 800, 900, 600, 538) #AC toten bardziej na wierzchu
#' )
#'
#' #preparing the styles data frame
#' df_styles <- data.frame(
#'  AC = c("actual","actual","actual","actual","actual"),
#'  PL = c("plan","plan","plan","plan","plan"),
#'  triangles = c("previous", "previous","previous","previous","previous"))
#'
#' #creating the svg string
#' barchart_grouped <- bar_chart_grouped(data,
#'  data$city,  "AC", "PL","triangles", c("triangles", "AC", "PL"), df_styles)
#'
#' #showing the plot
#' barchart_grouped
#'
bar_chart_grouped <- function(data, cat, foreground, background, markers=NULL, series_labels, styles = NULL){
  svg_string <- initialize(width = 320 + get_margins()$left, height= get_margins()$top + 24*length(cat) + get_margins()$top) %>%
    draw_bars_grouped(data, cat, foreground, background, markers, series_labels, df_styles = styles) %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}
