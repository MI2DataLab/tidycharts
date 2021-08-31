
add_horiz_waterfall_bars <-
  function(svg_string,
           y,
           values,
           pos_color = "rgb(64,64,64)",
           neg_color = "black") {
    max_bar_width <- 200
    label_offset <- 4.8
    max_val <- max(abs(cumsum(values)))
    min_val <- abs(min(cumsum(values)))
    label_x <- min_val/max_val * max_bar_width + max(str_width(format(abs(values), digits = 3)))
    y_axis_pos <- max(str_width(y)) + 10 + label_x + label_offset
    prev_level <- y_axis_pos
    starting_y <- 50


    for (i in 1:length(y)) {
      bar_width <- values[i] / max_val * max_bar_width
      y_pos <- starting_y + 24 * (i - 1) + 4
      # starting y pos + category width * number of plotted categories + 1/6 * category height

      if (bar_width > 0) {
        x_pos <- prev_level
        bar_w <- bar_width
        line_x <- x_pos + bar_w - 0.48
        text_x <- x_pos + bar_w + label_offset
        text_aligment <- "start"
      }
      else{
        x_pos <- prev_level + bar_width
        bar_w <- -1 * bar_width
        line_x <- x_pos
        text_x <- x_pos - label_offset
        text_aligment <- "end"
      }
      # add y axis
      svg_string <- draw_bar(
        svg_string,
        x = y_axis_pos,
        y = y_pos - 4,
        width = 1.6,
        height = 24,
        color = "black"
      )
      # add bar
      svg_string <- draw_bar(
        svg_string,
        x = x_pos,
        y = y_pos,
        height = 16,
        width = bar_w,
        color = choose_waterfall_color(bar_width, pos_color, neg_color)
      )

      # add label

      svg_string <- draw_text(
        svg_string,
        text = round(abs(values[i]), 3),
        x = text_x,
        y = y_pos + 8 + 4,
        text_anchor = text_aligment
      )

      # add line after every bar but not last
      if (i < length(y)) {
        svg_string <- draw_bar(
          svg_string = svg_string,
          x = line_x,
          y = y_pos + 16,
          height = 8,
          width = 0.48,
          color = "black"
        )
      }

      # add y axis labels
      svg_string <- draw_text(
        svg_string,
        text = y[i],
        x = y_axis_pos - 4.8 - label_x,
        y = y_pos + 12,
        text_anchor = "end"
      )

      prev_level <- prev_level + bar_width
    }

    return(svg_string)
  }


add_result_bar <- function(svg_string,
                           y,
                           values,
                           title,
                           pos_color = "rgb(64,64,64)",
                           neg_color = "black") {
  max_bar_width <- 200
  label_offset <- 4.8
  max_val <- max(abs(cumsum(values)))
  min_val <- abs(min(cumsum(values)))
  label_x <- min_val/max_val * max_bar_width + max(str_width(format(abs(values), digits = 3)))
  y_axis_pos <- max(str_width(y)) + 10 + label_x  + label_offset
  starting_y <- 50
  y_pos <- length(y) * 24 + starting_y + 4
  value <- sum(values)
  bar_width <- value / max_val * max_bar_width

  if (value > 0) {
    x_pos <- y_axis_pos
    bar_w <- bar_width
    line_x <- x_pos + bar_w - 0.48
    text_x <- x_pos + bar_w + label_offset
    text_aligment <- "start"
  }
  else{
    x_pos <- y_axis_pos + bar_width
    bar_w <- -1 * bar_width
    line_x <- x_pos
    text_x <- x_pos - label_offset
    text_aligment <- "end"
  }

  # add y axis
  svg_string <- draw_bar(
    svg_string,
    x = y_axis_pos,
    y = y_pos - 4,
    width = 1.6,
    height = 24,
    color = "black"
  )
  # draw bar
  svg_string <- draw_bar(
    svg_string,
    x = x_pos,
    y = y_pos,
    color = choose_waterfall_color(value, pos_color, neg_color),
    height = 16,
    width = bar_w
  )
  # draw line
  svg_string <- draw_bar(
    svg_string = svg_string,
    x = line_x,
    y = y_pos - 8,
    height = 8,
    width = 0.48,
    color = "black"
  )
  # add y label
  svg_string <- draw_text(
    svg_string,
    text = title,
    x = y_axis_pos - 4.8 - label_x,
    y = y_pos + 12,
    text_anchor = "end"
  )
  # add value label
  svg_string <- draw_text(
    svg_string,
    text = format(abs(value), digits = 3),
    x = text_x,
    y = y_pos + 8 + 4,
    text_anchor = text_aligment
  )

  return(svg_string)
}

#' Generate horizontal waterfall chart.
#'
#' @param add_result boolean value if result bar should be plotted
#' @param result_title the title for the result bar. Ignored if add_result is false
#'
#' @inheritParams bar_chart
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' df <- data.frame(
#'   city = c("Berlin", "Munich", "Cologne", "London", "Vienna", "Paris", "Zurich"),
#'   profit = sin(1:7)
#' )
#'
#' bar_chart_waterfall(cat = 'city', series = 'profit', data = df)
bar_chart_waterfall <-
  function(cat,
           series,
           data = NULL,
           add_result = FALSE,
           result_title = NULL) {
    stopifnot(length(cat) == length(series))

    . <- NULL # initialize . variable not to get CRAN notes

    if (!is.null(data)) {
      cat <- get_vector(data, cat)
      series <- get_vector(data, series)
    }
    if (add_result) {
      labels <- c(cat, result_title)
    } else{
      labels <- cat
    }
    svg_string <- initialize(y_vector = labels,
               bar_width = 16) %>%
      add_horiz_waterfall_bars(cat, series) %>%
      {
        ifelse(add_result,
               add_result_bar(., cat, series, result_title),
               .)
      } %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }
