show <- function(svg_string) {
  magick::image_read_svg(svg_string, width = 500)
}


initialize <-
  function(svg_string = "",
           transformation = "",
           svg_string_append = "",
           width = 1000,
           height = 500,
           x_vector = NULL,
           bar_width = NULL,
           x_margin = 50,
           y_vector = NULL,
           y_margin = 50) {
    if (!is.null(x_vector) & !is.null(bar_width)) {
      width <- length(x_vector) * 1.5 * bar_width + x_margin
    }
    if (!is.null(y_vector) & ! is.null(bar_width)) {
      height <- length(y_vector) * 1.5 * bar_width + y_margin
    }
    return(
      paste(
        svg_string,
        paste0(
          '<svg  version="1.1"
          baseProfile="full"
          width="',
          width,
          '" height="',
          height,
          '" transform="',
          transformation,
          '" >'
        ),
        '<pattern id="diagonalHatch" patternUnits="userSpaceOnUse" width="4" height="4">
            <rect x="0" y="0" width="4" height="4" fill="white"/>
            <path d="M-1,1 l2,-2
                     M0,4 l4,-4
                     M3,5 l2,-2"
            style="stroke:rgb(64,64,64); stroke-width:1" />
            </pattern>',
        svg_string_append,
        sep = "\n"
      )
    )
  }


finalize <- function(svg_string) {
  return(paste(svg_string, '</svg>', sep = "\n"))
}


#' Add IBCS complient legend
#'
#' @param svg_string one element character vector containing SVG graphic statements. Legend will be added to this plot.
#' @param line1 first line of title. It should represent object of the plot
#' @param line2_measure First part of second line of the title. It wil be in bold text. It should represent buisness measure being analyzed.
#' @param line2_rest Second part of second line of the title. It should represent units of measure.
#' @param line3 Third line of the title, it should indicate time, scenarios, variances, etc
#'
#' @return one element character vector containing SVG graphic statements. svg_string with appended legend.
#' @export
#'
#' @examples

add_title <- function(svg_string, line1, line2_measure, line2_rest, line3=""){
  initialize(svg_string_append = svg_string) %>%
    draw_text(text = line1, x = 0, y = 12, text_anchor = "start") %>%
    draw_text(text = line2_measure, x = 0, y = 24, text_anchor = "start", text_weight = "bold") %>%
    draw_text(text = line2_rest, x = str_width(line2_measure, bold = T)+2, y = 24, text_anchor = "start") %>%
    draw_text(text = line3, x = 0, y = 36, text_anchor = "start") %>%
    finalize() %>%
    return()
}
