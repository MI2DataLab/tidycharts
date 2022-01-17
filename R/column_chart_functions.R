
add_column_bar <-
  function(svg_string,
           df,
           i,
           x_pos,
           x_label,
           series,
           max_height,
           bar_width,
           color = NULL,
           style = NULL,
           translate_vec = c(0,0)) {

    x_axis_pos <- get_x_axis_pos(df[series])
    bar_height = 0 # height of existing bar, updated in the loop

    for (j in 1:length(series)) {
      column_name <- series[j]
      height <- df[i, column_name] * 200 / max_height
      if (is.null(color)) {
        colors_ <- get_color_stacked(j)
      }
      else{
        colors_ <- list(bar_color = color,
                        text_color = "black")
      }

      # add bar
      svg_string <- draw_bar(
        svg_string = svg_string,
        x = x_pos + bar_width / 4,
        y = x_axis_pos - height - bar_height,
        height = height,
        width = bar_width,
        color = colors_$bar_color,
        style = style,
        translate_vec = translate_vec
      )
      bar_height <- bar_height + height

      # numeric value on bar
      if (abs(height) > 1.5 * 12 & length(series) > 1) {
        # height at least 150% of font size and more than one series
        svg_string <- draw_text(
          svg_string = svg_string,
          text = formatC(
            round_preserve_sum(as.numeric(df[i, series]),digits = 2)[j],
            digits=2,
            format='f'),
          x = x_pos + bar_width * 1.5 / 2,
          y = x_axis_pos - bar_height + (height / 2) + 6,
          text_color = colors_$text_color,
          translate_vec = translate_vec
        )
      }
    }
    return(svg_string)
  }




#' add bars to svg string
#'
#' @param svg_string the svg string to br appended, need to be finalized after
#' @param df data to be plotted - data frame in wide format
#' @param x vector to be on x axis
#' @param series character vector of column names representing series to split bars by it
#' @param bar_width the width of plotted bar
#' @param styles vector of styles of the bars
#' @param x_offset how much bars should be offset to the right (negative value means offsetting to the left)
#' @param translate vector of translation of the bars from the origin
#' @param add_x_axis boolean flag, if true automatically adds x axis with label
#' @param color optional custom color of the bars series, in svg string format, ie.: "rgb(223,12,121)" or "black"
#' @param add_legend boolean flag if legend should be added
#' @param legend_position string with legend position
#' @param max_val maximal value that bars will be scaled to
#'
#' @return svg string with added bars
#'
add_bars <-
  function(svg_string,
           df,
           x,
           series,
           bar_width,
           styles = NULL,
           x_offset = 0,
           translate = c(0, 0),
           add_x_axis = TRUE,
           color = NULL,
           add_legend = FALSE,
           legend_position = "left_top",
           max_val = NULL) {

    # TODO check series lengths and NA there
    n_bars <- length(x)
    left_margin <- get_margins()$left

    n_splits = length(series)
    x_axis_pos <- get_x_axis_pos(df[series], max_val)
    max_height <- ifelse(is.null(max_val),
                         max(abs(rowSums(df[series]))),
                         max_val)

    sums <- rowSums(df[series])
    for (i in 1:n_bars) {
      x_label <- substr(x[i], 1, 7)
      x_pos <- 1.5 * bar_width * (i - 1) + x_offset + left_margin
      svg_string <- add_column_bar(
        svg_string,
        df = df,
        i = i,
        x_pos = x_pos,
        x_label = x_label,
        series = series,
        max_height = max_height,
        bar_width,
        color = color,
        style = styles[i],
        translate_vec = translate
      )
      if (add_x_axis) {
        # add label on x axis
        svg_string <- draw_text(
          svg_string = svg_string,
          text = x_label,
          x = x_pos + bar_width * 1.5 / 2,
          y = x_axis_pos + sign(sums[i]) * 4.8 + ifelse(sums[i] > 0, 12, 0),
          translate_vec = translate
          # if total value of the bar is negative -> x_label is on top of x axis
        )

        # add x axis
        svg_string <- draw_x_axis(
          svg_string = svg_string,
          x = x_pos,
          y = x_axis_pos,
          bar_width = bar_width,
          line_width = 1.6,
          translate_vec = translate
        )
      }
    }
    if (add_legend == TRUE) {
      legend_pos <- switch (
        legend_position,
        "left_top" = c(
          x_offset + translate[1] + left_margin,
          x_axis_pos - df[1, series] / max_height * 200 + 6 + translate[2]
        )
      )
      svg_string <-
        draw_text(svg_string, series, legend_pos[1], legend_pos[2], text_anchor = "end")
    }
    return(svg_string)
  }



add_first_bar <- function(svg_string,
                          x_label,
                          value,
                          x_axis_pos = 250,
                          max_bar_height = 200,
                          top_value,
                          low_value,
                          bar_width,
                          color = "rgb(166,166,166)",
                          label_color = "black") {
  left_margin <- get_margins()$left
  x_pos <- 50 - bar_width * 1.5 + left_margin
  bar_height <- value / top_value * max_bar_height
  # calculate x labels y position
  x_label_pos <- ifelse(
    low_value >= 0,
    x_axis_pos + 4.8 + 10,
    x_axis_pos - low_value / top_value * max_bar_height + 25
  )
  # add axis on the bottom
  svg_string <- draw_x_axis(
    svg_string = svg_string,
    x = x_pos,
    y = x_axis_pos,
    bar_width = bar_width
  )
  # add label on axis
  svg_string <- draw_text(
    svg_string = svg_string,
    text = x_label,
    x = x_pos + 0.75 * bar_width,
    y = x_label_pos
  )

  if (bar_height >= 0) {
    bar_y <- x_axis_pos  - bar_height
    bar_h <- ceiling(bar_height)
    line_y <- x_axis_pos - bar_height
    label_y <- x_axis_pos - bar_height / 2 + 6
  } else{
    bar_y <- x_axis_pos
    bar_h <- -1 * bar_height
    line_y <- floor(x_axis_pos - bar_height)
    label_y <- x_axis_pos - bar_height / 2 + 6
  }
  # draw bar
  svg_string <-
    draw_bar(
      svg_string = svg_string,
      x = x_pos + bar_width / 4,
      y = bar_y,
      height = bar_h,
      width = bar_width,
      color = color
    )
  svg_string <- draw_bar(
    svg_string = svg_string,
    x = x_pos + 1.25 * bar_width,
    y = line_y,
    height = 0.2,
    width = bar_width / 2,
    color = "black"
  )
  # add label to bar
  svg_string <- draw_text(
    svg_string = svg_string,
    text = format(value, digits = 5),
    x = x_pos + 0.75 * bar_width,
    y = label_y,
    text_color = label_color
  )
  return(svg_string)
}


#' Add waterfall style bars to the column chart
#'
#' @param svg_string the svg string to br appended, need to be finalized after
#' @param df data to be plotted - data frame in wide format
#' @param x vector to be on x axis
#' @param series character vector of column names representing series to split bars by it
#' @param bar_width the width of plotted bar
#' @param styles vector of styles of the bars
#' @param pos_color color to be associated with positive values (in string format)
#' @param neg_color color to be associated with negative values (in string format)
#' @param add_result_bar boolean flag to add result bar as the last bar or not.
#' @param result_bar_pos flag indicating position of the result bar. 1 - bar offset 1/9 category width right from the last bar. 2 - result bar as completely new bar. If add_result_bar is false, it is ignored.
#' @param positive_prefix how to indicate positive value, ie. "+" or ""(empty string).
#' @param result_bar_color color of result bar. If add_result_bar is false, it is ignored.
#' @param result_title title of result bar to be on x axis. If add_result_bar is false, it is ignored.
#' @param ref_value first bar starts from this value, intended to be used with add_first_bar function.
#' @param translate_vec 2 element translation vector. By setting this parameter you can translate bars and legend.
#'
#' @return svg string with appended waterfall bars
#'
add_waterfall_bars <-
  function(svg_string,
           df,
           x,
           series,
           bar_width,
           styles = NULL,
           pos_color = "rgb(64,64,64)",
           neg_color = "black",
           add_result_bar = TRUE,
           result_bar_pos = "1",
           positive_prefix = "",
           result_bar_color = NULL,
           result_title = NULL,
           ref_value = 0,
           translate_vec = c(0,0)) {
    # NOT IMPLEMENTED translation along y axis
    x_axis_pos <- get_x_axis_pos(df[series])
    max_bar_height <- 200
    top_value <- max(abs(df[series]))
    prev_level <- ref_value / top_value * max_bar_height
    left_margin <- get_margins()$left + translate_vec[1]

    # calculate x labels y position
    low_value <- min(df[series])
    x_label_pos <- ifelse(
      low_value >= 0,
      x_axis_pos + 4.8 + 10,
      x_axis_pos - low_value / top_value * max_bar_height + 25
    )

    for (i in 1:length(x)) {
      bar_top_pos <- df[i, series] / top_value * max_bar_height
      bar_height <- bar_top_pos - prev_level
      x_pos = 1.5 * bar_width * (i - 1) + left_margin

      if (i == 1)
        actual_delta <- df[1, series] - ref_value
      else{
        actual_delta <- df[i, series] - df[i - 1, series]
      }


      # add axis on the bottom
      svg_string <- draw_x_axis(
        svg_string = svg_string,
        x = x_pos,
        y = x_axis_pos,
        bar_width = bar_width
      )

      # add label on axis
      svg_string <- draw_text(
        svg_string = svg_string,
        text = x[[i]],
        x = x_pos + 0.75 * bar_width,
        y = x_label_pos
      )

      if (bar_height >= 0) {
        bar_y <- ceiling(x_axis_pos - prev_level - bar_height)
        bar_h <- bar_height
        line_y <- bar_y - 0.2 # pos - height of line
        label_y <- x_axis_pos - prev_level - bar_height - 4.8
        last_label_y <- label_y
        last_label_x <- x_pos + 0.75 * bar_width
        last_label_anchor <- "middle"
        text_prefix <- positive_prefix
      } else{
        bar_y <- x_axis_pos - prev_level
        bar_h <- -1 * bar_height
        line_y <- x_axis_pos - prev_level - bar_height
        label_y <- x_axis_pos - prev_level - bar_height + 4.8 + 6
        last_label_y <- label_y - 10.8
        last_label_x <- x_pos + 1.25 * bar_width + 4.8
        last_label_anchor <- "left"
        text_prefix <- ""
      }
      # draw bar
      svg_string <-
        draw_bar(
          svg_string = svg_string,
          x = x_pos + bar_width / 4,
          y = bar_y,
          height = bar_h,
          width = bar_width,
          color = choose_waterfall_color(bar_height, pos_color, neg_color),
          style = styles[i]
        )
      # draw line but not after the last one
      if (i < length(x)) {
        svg_string <- draw_bar(
          svg_string = svg_string,
          x = x_pos + 1.25 * bar_width,
          y = line_y,
          height = 0.2,
          width = bar_width / 2,
          color = "black"
        )
        # add label to bar
        svg_string <- draw_text(
          svg_string = svg_string,
          text = paste0(text_prefix, format(actual_delta, digits = 6)),
          x = x_pos + 0.75 * bar_width,
          y = label_y,
          text_color = "black"
        )
      } else{
        # add label next to last bar
        svg_string <- draw_text(
          svg_string = svg_string,
          text = paste0(positive_prefix, format(actual_delta, digits = 6)),
          x = last_label_x,
          y = last_label_y,
          text_anchor = last_label_anchor,
          text_color = "black"
        )
      }
      prev_level <- prev_level + bar_height
    }
    if (add_result_bar) {
      offset <- switch(result_bar_pos,
                       "1" = bar_width / 6,
                       "2" = 1.5 * bar_width)# 1/9 category width, so 1/6 bar width
      result_hight <- prev_level
      color <- ifelse(
        is.null(result_bar_color),
        choose_waterfall_color(result_hight, pos_color, neg_color),
        result_bar_color
      )
      # add result bar
      svg_string <-
        draw_bar(
          svg_string = svg_string,
          x = x_pos + bar_width / 4 + offset,
          y = x_axis_pos - result_hight,
          height = abs(result_hight),
          width = bar_width,
          color = color
        )
      # add label on result bar
      svg_string <- draw_text(
        svg_string = svg_string,
        text = format(df[[length(x), series]], digits = 5),
        x = x_pos + offset + bar_width * 0.75,
        y = x_axis_pos - result_hight / 2,
        text_color = "white"
      )

      if (offset > bar_width) {
        # add reference line
        svg_string <- draw_bar(
          svg_string = svg_string,
          x = x_pos + bar_width / 4 + bar_width,
          y = x_axis_pos - result_hight,
          height = 0.2,
          width = bar_width / 2,
          color = "black"
        )
        # add axis on the bottom
        svg_string <- draw_x_axis(
          svg_string = svg_string,
          x = x_pos + offset,
          y = x_axis_pos,
          bar_width = bar_width
        )

        # add label on axis
        svg_string <- draw_text(
          svg_string = svg_string,
          text = result_title,
          x = x_pos + offset + 0.75 * bar_width,
          y = x_label_pos
        )
      }
    }
    return(svg_string)
  }


add_abs_variance_bars <-
  function(svg_string,
           x,
           baseline,
           real,
           colors,
           bar_width,
           x_title,
           x_style) {
    x_pos <- get_margins()$left


    color <- choose_variance_colors(colors)
    max_val <- max(abs(baseline), abs(real))
    variance <- real - baseline
    x_axis_pos <- get_x_axis_pos_abs_variance(baseline, real)

    # add legend on the left of plot
    first_bar_h <- variance[1] / max_val * 200

    svg_string <- draw_text(
      svg_string,
      text = paste0("\u0394 ", x_title),
      x = x_pos,
      y = x_axis_pos - first_bar_h / 2 + 6,
      text_anchor = "end"
    )

    for (i in 1:length(x)) {
      # add axis
      svg_string <- draw_bar(
        svg_string = svg_string,
        x = x_pos,
        y = x_axis_pos,
        height = 4.8,
        width = 1.5 * bar_width,
        color = "rgb(166,166,166)",
        style = x_style
      )

      bar_h <- abs(variance[i] / max_val * 200)

      if (variance[i] > 0) {
        c <- color[["pos_color"]]
        bar_y <- x_axis_pos - bar_h + 2.4
        label_y <- bar_y - 4.8
        label_text <- paste("+", format(variance[i], digits = 4))
        x_label_y <- x_axis_pos + 4.8 + 12
      } else if (variance[i] < 0) {
        c <- color[["neg_color"]]
        bar_y <- x_axis_pos + 2.4
        label_y <- bar_y + bar_h + 4.8 + 9
        label_text <-format(variance[i], digits = 4)
        x_label_y <- x_axis_pos - 2.4
      } else{
        c <- "rgb(128,128,128)" # neutral gray
        bar_y <- x_axis_pos + 2.4
        label_y <- bar_y - 4.8
        label_text <- format(variance[i], digits = 4)
        x_label_y <- x_axis_pos + 4.8 + 12
      }

      # add bar
      svg_string <- draw_bar(
        svg_string,
        x = x_pos + 0.25 * bar_width,
        y = bar_y,
        height = bar_h,
        width = bar_width,
        color = c
      )
      # add label
      svg_string <- draw_text(
        svg_string,
        text = label_text,
        x = x_pos + 0.75 * bar_width,
        y = label_y
      )
      # add x label
      svg_string <- draw_text(
        svg_string,
        text = x[i],
        # TODO formatting xlabel
        x = x_pos + 0.75 * bar_width,
        y = x_label_y
      )
      x_pos <- x_pos + bar_width * 1.5
    }
    return(svg_string)
  }


add_relative_variance_pins <-
  function(svg_string,
           x,
           baseline,
           real,
           colors,
           bar_width,
           x_title,
           x_style,
           translate = c(0,0),
           styles = NULL) {
    x_axis_pos <- 200
    color <- choose_variance_colors(colors)
    x_pos <- get_margins()$left

    values <- real / baseline * 100 - 100
    max_val <- 100
    # add legend on the left of plot
    first_bar_h <- values[1] / max_val * 100

    svg_string <- draw_text(
      svg_string,
      text = paste0("\u0394 ", x_title), # delta sign = \u0394
      x = x_pos + translate[1] + bar_width * 0.5,
      y = x_axis_pos - first_bar_h / 2 + 6,
      text_anchor = "end"
    )
    for (i in 1:length(x)) {
      # add axis
      svg_string <- draw_bar(
        svg_string = svg_string,
        x = x_pos,
        y = x_axis_pos,
        height = 4.8,
        width = 1.5 * bar_width,
        color = "rgb(166,166,166)",
        translate_vec = translate,
        style = x_style
      )

      bar_h <- abs(values[i] / max_val * 100)

      if (values[i] > 0) {
        c <- color[["pos_color"]]
        bar_y <- x_axis_pos - bar_h + 2.4
        marker_y <- bar_y - 5.6
        label_y <- bar_y - 4.8 - 5
        label_text <- paste("+", round(values[i]))
        x_label_y <- x_axis_pos + 4.8 + 12
      } else if (values[i] < 0) {
        c <- color[["neg_color"]]
        bar_y <- x_axis_pos + 2.4
        marker_y <- bar_y + bar_h - 5.6
        label_y <- bar_y + bar_h + 4.8 + 12
        label_text <- round(values[i])
        x_label_y <- x_axis_pos - 2.4
      } else{
        c <- "rgb(128,128,128)" # neutral gray
        bar_y <- x_axis_pos + 2.4
        marker_y <- bar_y - 5.6
        label_y <- bar_y - 4.8 - 5
        label_text <- round(values[i])
        x_label_y <- x_axis_pos + 4.8 + 12
      }

      # add box
      svg_string <- draw_bar(
        svg_string,
        x = x_pos + 0.75 * bar_width - 5.6,
        y = marker_y,
        height = 11.2,
        width = 11.2,
        style = styles[i],
        translate_vec = translate
      )
      # add bar
      svg_string <- draw_bar(
        svg_string,
        x = x_pos + 0.75 * bar_width - 2.4,
        y = bar_y,
        height = bar_h,
        width = 4.8,
        color = c,
        translate_vec = translate
      )
      # add label
      svg_string <- draw_text(
        svg_string,
        text = label_text,
        x = x_pos + 0.75 * bar_width,
        y = label_y,
        translate_vec = translate
      )
      # add x label
      svg_string <- draw_text(
        svg_string,
        text = x[i],
        # TODO formatting xlabel
        x = x_pos + 0.75 * bar_width,
        y = x_label_y,
        translate_vec = translate
      )
      x_pos <- x_pos + bar_width * 1.5
    }
    return(svg_string)
  }


add_triangles <- function(svg_string,
                          df,
                          x,
                          bar_width,
                          series,
                          x_offset = 0,
                          translate = c(0, 0),
                          max_val = NULL,
                          add_legend = FALSE,
                          styles = NULL) {
  x_axis_pos <- get_x_axis_pos(df[series], max_val)
  max_height <- ifelse(is.null(max_val), max(df[series]), max_val)
  left_margin <- get_margins()$left
  for (i in 1:length(x)) {
    x_pos <- 1.5 * bar_width * (i - 1) + 4 + 0.25 * bar_width + left_margin

    svg_string <- draw_triangle(
      svg_string = svg_string,
      tip_position_x = x_pos,
      tip_position_y = x_axis_pos - df[i, series] / max_height * 200,
      orientation = "right",
      style = styles[i]
    )
  }
  if (add_legend == TRUE) {
    legend_pos <- c(x_offset + translate[1] + left_margin,
                    x_axis_pos - df[1, series] / max_height * 200 + 6 + translate[2])
    svg_string <-
      draw_text(svg_string, series, legend_pos[1], legend_pos[2], text_anchor = "end")
  }
  return(svg_string)
}


add_legend <- function(svg_string, df, x, series, bar_width) {
  x_axis_pos <- get_x_axis_pos(df[series])
  left_margin <- get_margins()$left
  if (length(series) == 1)
    return(svg_string)
  x_pos = 1.5 * bar_width * length(x) + 4.8 - bar_width / 4 + left_margin
  max_height = max(abs(rowSums(df[series])))
  total_height = 0
  for (column_name in series) {
    bar_height <- df[length(x), column_name] * 200 / max_height
    label_height <- bar_height / 2

    svg_string <- draw_text(
      svg_string = svg_string,
      text = column_name,
      x = x_pos,
      y = x_axis_pos - label_height - total_height + 6,
      text_anchor = "left"
    )

    total_height <- total_height + bar_height
  }
  return(svg_string)
}


add_top_values <-
  function(svg_string,
           df,
           x,
           series,
           bar_width,
           labels = NULL,
           translate = c(0, 0),
           max_val = NULL,
           ref_value = NULL) {
    x_axis_pos <- get_x_axis_pos(df[series], max_val)

    heights <- rowSums(df[series])
    max_height <-
      ifelse(is.null(max_val), max(abs(heights)), max_val)
    left_margin <- get_margins()$left

    ref_value <-
      ifelse(is.null(ref_value), max_height, ref_value)

    if (is.null(labels)) {
      round_df <- apply(df[series], MARGIN = 1, round_preserve_sum, digits = 2)
      if(!is.null(dim(round_df))) labels <- rowSums(t(round_df))
      else labels <- round_df
    }
    if (length(labels) == 1 && labels == "percent"){
      labels <- paste0(format(heights / ref_value * 100, digits = 3),"%")
    }
    else{
      labels <-labels
    }

    for (i in 1:length(x)) {
      x_pos <- 1.5 * bar_width * (i - 1) + left_margin
      bar_height <- heights[i] * 200 / max_height

      # numeric value label for total bar
      svg_string <- draw_text(
        svg_string = svg_string,
        text = formatC(labels[i], digits = 2, format = 'f'),
        x =  x_pos + bar_width * 1.5 / 2,
        y = x_axis_pos - bar_height - sign(bar_height) * 4.8 + ifelse(bar_height > 0, 0, 6)
      )
    }
    return(svg_string)
  }


normalize_df <- function(df, max_bar_height) {
  return(df / max(df) * max_bar_height)
}

reference <- function(df, x, series, ref_value) {
  new_df <- data.frame(df)
  new_df[series] <- new_df[series] /  ref_value * 100
  return(new_df)
}

get_plot_height <- function(df_num, x_axis_pos = get_x_axis_pos(df_num), max_bar_height = 200){
  min_val <- min(df_num)
  max_val <- max(df_num)
  if(abs(min_val) > max_val){
    return(x_axis_pos + max_bar_height + 50) # axis position + height of the longest negative bar + 50 margin
  }else if(min_val < 0){
    return(x_axis_pos + abs(min_val) / max_val * max_bar_height + 50)
  }else{
    return(x_axis_pos + 50)
  }
}

get_plot_height_abs_var <- function(real, baseline){
  max_bar_height <- 200
  top_margin <- get_margins()$top

  x_axis_pos <- get_x_axis_pos_abs_variance(baseline, real)
  max_val <- max(abs(baseline), abs(real))
  variance <- real - baseline
  highest_bar <- max(variance) / max_val * max_bar_height
  lowest_bar <- min(variance) / max_val * max_bar_height
  if(abs(lowest_bar) > max_val){
    return(x_axis_pos + abs(lowest_bar) + 50) # axis position + height of the longest negative bar + 50 margin
  }else if(lowest_bar < 0){
    return(x_axis_pos + abs(lowest_bar) + 50)
  }else{
    return(x_axis_pos + 50)
  }
}

get_x_axis_pos <- function(df_num, max_val = NULL){
  max_bar_height <- 200
  top_margin <- get_margins()$top
  min_val <- min(df_num)
  max_val <- max(df_num)
  longest_bar <- max(abs(min_val), abs(max_val))
  if (max_val > 0) {
    return(top_margin + max_bar_height * max_val / longest_bar)
  }
  else{
    return(top_margin)
  }
}

get_x_axis_pos_abs_variance <- function(baseline, real){
  max_bar_height <- 200
  top_margin <- get_margins()$top
  max_val <- max(abs(baseline), abs(real))
  variance <- real - baseline
  highest_bar <- max(variance) / max_val * max_bar_height
  lowest_bar <- min(variance) / max_val * max_bar_height
  if (highest_bar < 0) {
    highest_bar <- 0
  }
  return(top_margin + highest_bar)

}

#' Generate basic column chart.
#'
#' If more than one series is supplied, stacked column plot is generated.
#'
#' @param data data frame in wide format containing data to be plotted
#' @param x vector containing labels for x axis or name of column in data with values of x axis labels
#' @param series vector containing names of columns in data with values to plot
#' @param series_labels optional vector with labels for series to be plotted as legend. The default is the same as series.
#' @param styles optional vector with styles of bars
#' @param interval intervals on x axis. The width of the bars depends on this parameter
#'
#' @inherit bar_chart return
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # prepare some data frame
#' df <- data.frame(x = month.abb[1:6],
#'                  y = c(2, 4, 2, 1, 2.5, 3),
#'                  z = c(3, 4.5, 2, 1, 4, 2))
#'
#' # generate character vectors with svg data
#' svg1 <- column_chart(df, x = 'x', series = 'y')
#' svg2 <- column_chart(df, x = df$x, series = c('y', 'z'))
#'
#'
#' # show the plot
#' svg1
#'
column_chart <- function(data, x, series = NULL, series_labels = series, styles = NULL, interval = 'months') {
  bar_width <- get_interval_width(interval)$bar_width
  stop_if_many_series(series, max_series = 6) # maximum 6 series
  stop_if_pos_neg_values(data, series) # signum of values in one bar is the same for every bar
  x <- get_vector(data, x)
  stop_if_many_categories(x, max_categories = 24)
  svg_string <- initialize(x_vector = x, bar_width = bar_width, height = get_plot_height(data[series])) %>%
    add_bars(data, x, series, bar_width = bar_width, styles) %>%
    add_legend(data, x, series_labels, bar_width = bar_width) %>%
    add_top_values(data, x, series, bar_width = bar_width) %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}


#' Generate column chart with normalization.
#'
#' Every column will be rescaled, so columns have the same height.
#'
#' @inheritParams column_chart
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' # prepare some data frame
#' df <- data.frame(x = month.abb[1:6],
#'                  y = c(2, 4, 2, 1, 2.5, 3),
#'                  z = c(3, 4.5, 2, 1, 4, 2))
#'
#' # generate character vector with svg data
#' column_chart_normalized(df, x = df$x, series = c('y', 'z'))
#'
column_chart_normalized <- function(data, x, series = NULL, series_labels = series, interval = 'months') {
  bar_width <- get_interval_width(interval)$bar_width

  x <- get_vector(data, x)
  stop_if_many_series(series, max_series = 6) # maximum 6 series
  stop_if_many_categories(x, max_categories = 24)

  normalized_df <- normalize_rows(data, x, series)

  svg_string <- initialize(x_vector = x, bar_width = bar_width, height = 300) %>%
    add_bars(normalized_df, x, series, bar_width = bar_width) %>%
    add_legend(normalized_df, x, series_labels, bar_width = bar_width) %>%
    draw_ref_line_horizontal(x, bar_width = bar_width, line_y = get_margins()$top, label = "100") %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}


#' Generate column chart with reference line.
#'
#' @inheritParams column_chart
#' @param ref_value one element numeric vector with referencing value.
#' @param ref_label name of the referencing value
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' # prepare some data frame
#' df <- data.frame(x = month.abb[1:6],
#'                  y = c(2, 4, 2, 1, 2.5, 3),
#'                  z = c(3, 4.5, 2, 1, 4, 2))
#'
#' # generate character vector with svg data
#' column_chart_reference(df, x = 'x',
#'                        series = 'y',
#'                        ref_value = 3,
#'                        ref_label = 'baseline')
#'
column_chart_reference <-
  function(data,
           x,
           series,
           ref_value,
           ref_label = NULL,
           styles = NULL,
           interval = 'months') {
    bar_width <- get_interval_width(interval)$bar_width


    stop_if_many_series(series, max_series = 1) # maximum 1 series
    x <- get_vector(data, x)
    stop_if_many_categories(x, max_categories = 24)

    x_axis_pos <- get_x_axis_pos(data[series])

    ref_label <- ifelse(is.null(ref_label), ref_value, ref_label)
    referenced_df <- reference(data, x, series, ref_value)
    index_level <-
      ref_value / max(data[series]) * 200
    svg_string <- initialize(
      x_vector = x,
      bar_width = bar_width,
      height = get_plot_height(referenced_df[series], x_axis_pos = x_axis_pos)
    ) %>%
      add_bars(referenced_df,
               x,
               series,
               bar_width = bar_width,
               styles = styles) %>%
      draw_ref_line_horizontal(
        x,
        bar_width = bar_width,
        line_y = x_axis_pos - index_level,
        label = ref_label
      ) %>%
      add_top_values(data,
                     x,
                     series,
                     bar_width,
                     labels = "percent",
                     ref_value = ref_value) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }

#' Generate column waterfall chart for visualizing contribution.
#'
#' @inheritParams column_chart
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' df <- data.frame(x = 10:18,
#'                  y = rnorm(9))
#' column_chart_waterfall(df, 'x', 'y')
column_chart_waterfall <-
  function(data,
           x,
           series,
           styles = NULL,
           interval = 'months') {
    bar_width <- get_interval_width(interval)$bar_width

    x <- get_vector(data, x)

    stop_if_many_categories(x, max_categories = 24)
    stop_if_many_series(series, max_series = 1) # maximum 1 series

    svg_string <- initialize(
      x_vector = x,
      bar_width = bar_width,
      height = get_plot_height(data[series], x_axis_pos = 250)
    ) %>%
      add_waterfall_bars(data, x, series, bar_width, styles) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }

#' Generate column chart with absolute variance.
#'
#' Visualize variance between two time series (baseline and real) in the same units as the time series. Choose colors parameter accordingly to business interpretation of larger/smaller values.
#'
#' @param data data frame with columns containing data for x, baseline or real series
#' @param baseline vector containing base values or name of column in data with base values
#' @param real vector containing values that will be compared to baseline  or name of column in data with that values
#' @param colors 1 if green color represents positive values having good business impact and red negative values having bad impact or 2 if otherwise
#' @param x_title the title of the plot
#' @param x_style style of the x axis to indicate baseline scenario. The default is 'previous'.
#'
#' @inheritParams column_chart
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' x <- month.abb
#' baseline <- rnorm(12)
#' real <- c(rnorm(6, mean = -1), rnorm(6, mean = 1))
#' column_chart_absolute_variance(x, baseline, real, x_title = 'profit')
column_chart_absolute_variance <-
  function(x,
           baseline,
           real,
           colors = 1,
           data = NULL,
           x_title = "PY",
           x_style = 'previous',
           interval = 'months') {

    bar_width <- get_interval_width(interval)$bar_width

    if (!is.null(data)) {
      x <- get_vector(data, x)
      baseline <- get_vector(data, baseline)
      real <- get_vector(data, real)
    }

    stop_if_variance_colors(colors)
    stop_if_many_categories(x, max_categories = 24)

    svg_string <- initialize(
      x_vector = x,
      bar_width = bar_width,
      height = get_plot_height_abs_var(real, baseline)
    ) %>%
      add_abs_variance_bars(x, baseline, real, colors, bar_width, x_title, x_style) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }

#' Generate grouped column chart for visualizing up to 3 data series.
#'
#' @param foreground vector or name of column in data representing heights of bars visible in the foreground
#' @param background vector or name of column in data representing heights of bars visible in the background
#' @param markers optional vector representing position of triangles
#' @param series_labels vector of series titles. Consists of 2 or 3 elements
#' @param styles optional dataframe of styles. First column contains styles for foreground series, second for background, third for triangles. dim(styles) must be length(x), length(titles)
#'
#' @inheritParams column_chart
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' df <- data.frame(x = month.abb[7:12],
#'                  actual = rnorm(6, mean = 5, sd = 0.3),
#'                  budget = rnorm(6, mean = 4.5, sd = 0.7),
#'                  prev_year = rnorm(6, mean = 4))
#'
#' column_chart_grouped(x = df$x,
#'                      foreground = df$actual,
#'                      background = df$budget,
#'                      markers = df$prev_year,
#'                      series_labels = c('AC', 'BU', 'PY'))
column_chart_grouped <-
  function(x,
           foreground,
           background,
           markers = NULL,
           data = NULL,
           series_labels,
           styles = NULL,
           interval = 'months') {

    bar_width <- get_interval_width(interval)$bar_width
    translation_vec <- c(0,0) # c(max(str_width(series_labels)) + 10, 0)
    . <- NULL # initialize . variable not to get CRAN notes
    if (!is.null(data)) {
      x <- get_vector(data, x)
      foreground <- get_vector(data, foreground)
      background <- get_vector(data, background)
    }

    stopifnot(length(series_labels) >= 2)
    stop_if_many_categories(x, max_categories = 24)

    df <- data.frame(foreground, background)
    colnames(df) <- series_labels[1:2]

    if (!is.null(markers)) {
      markers <- get_vector(data, markers)
      stopifnot(length(series_labels) == 3)
      triangles_df <-  data.frame(markers)
      df <- cbind(df, triangles_df)
      colnames(df) <- series_labels
    }
    max_bar_height <- 200
    df <- normalize_df(df, max_bar_height)
    svg_string <- initialize(x_vector = x, bar_width = bar_width, height = get_plot_height(df[series_labels])) %>%
      add_bars(
        df[, series_labels[2], drop = FALSE],
        x = x,
        bar_width = bar_width,
        series = series_labels[2],
        x_offset = -(bar_width / 6),
        add_x_axis = FALSE,
        color = "rgb(166,166,166)",
        translate = translation_vec,
        add_legend = TRUE,
        max_val = max_bar_height,
        styles = styles[[2]]
      ) %>%
      add_bars(
        df[, series_labels[1], drop = FALSE],
        x = x,
        bar_width = bar_width,
        series = series_labels[1],
        translate = translation_vec,
        add_legend = TRUE,
        max_val = max_bar_height,
        styles = styles[[1]]
      ) %>%
      {
        ifelse(
          is.null(markers),
          . ,
          # pass svg_string if triangles is null
          add_triangles(
            # or add triangles
            .,
            df[, series_labels[3], drop = FALSE],
            x = x,
            bar_width = bar_width,
            series = series_labels[3],
            translate = translation_vec,
            max_val = max_bar_height,
            add_legend = TRUE,
            styles = styles[[3]]
          )
        )
      } %>%
      add_top_values(
        df,
        labels = foreground,
        x = x,
        series = series_labels[1],
        bar_width = bar_width,
        translate = translation_vec,
        max_val = max_bar_height
      ) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }


#' Generate column chart with relative variance (in percents).
#'
#' @inheritParams column_chart_absolute_variance
#' @param styles optional vector with styles of the pin heads
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' x <- month.abb
#' baseline <- rnorm(12, mean = 1, sd = 0.2)
#' real <- c(rnorm(6, mean = 0.8, sd = 0.2), rnorm(6, mean = 1.2, sd = 0.2))
#' column_chart_relative_variance(x, baseline, real, x_title = 'profit %')
column_chart_relative_variance <-
  function(x,
           baseline,
           real,
           colors = 1,
           data = NULL,
           x_title,
           x_style = 'previous',
           styles = NULL,
           interval = 'months') {
    stop_if_variance_colors(colors)

    if (!is.null(data)) {
      x <- get_vector(data, x)
      baseline <- get_vector(data, baseline)
      real <- get_vector(data, real)
    }

    stop_if_many_categories(x, max_categories = 24)

    bar_width <- get_interval_width(interval)$bar_width
    translation_vec = c(str_width(x_title), 0)
    svg_string <- initialize(x_vector = x,
               bar_width = bar_width,
               height = 300) %>%
      add_relative_variance_pins(
        x,
        baseline,
        real,
        colors,
        bar_width,
        x_title,
        x_style,
        translate = translation_vec,
        styles = styles
      ) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }


#' Generate column waterfall chart with absolute variance.
#'
#' @inheritParams column_chart_absolute_variance
#' @param result_title title for the result bar
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' x <- month.abb
#' baseline <- rnorm(12)
#' real <- c(rnorm(6, mean = -1), rnorm(6, mean = 1))
#' column_chart_waterfall_variance(x, baseline, real, result_title = 'year profit')
column_chart_waterfall_variance <-
  function(x, baseline, real, colors = 1, data = NULL, result_title, interval = 'months') {

    if (!is.null(data)) {
      x <- get_vector(data, x)
      baseline <- get_vector(data, baseline)
      real <- get_vector(data, real)
    }

    bar_width <- get_interval_width(interval)$bar_width
    stop_if_many_categories(x, max_categories = 24)

    difference <- real - baseline
    df <- data.frame("series" = difference)

    svg_string <- initialize(x_vector = x, bar_width = bar_width, height = get_plot_height(df)) %>%
      add_first_bar(
        x[1],
        df[1, 'series'],
        top_value = max(df['series']),
        low_value = min(df['series']),
        bar_width = bar_width,
        x_axis_pos = get_x_axis_pos(real - baseline)
      ) %>%
      add_waterfall_bars(
        df[-1, , drop = FALSE],
        x[-1],
        series = "series",
        bar_width,
        pos_color = choose_variance_colors(colors)[["pos_color"]],
        neg_color = choose_variance_colors(colors)[["neg_color"]],
        add_result_bar = TRUE,
        result_bar_pos = "2",
        positive_prefix = "+",
        result_bar_color = "rgb(64,64,64)",
        result_title = result_title,
        ref_value = df[1, 'series'],
        translate_vec = c(50,0)
      ) %>%
      finalize()
    class(svg_string) <- c('tidychart', 'character')
    return(svg_string)
  }
