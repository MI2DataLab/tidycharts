
#' Function to render SVG as bitmap, used only in tests
#'
#' @param svg_string string containing SVG statements
#'
#' @return No return value, called for side effects
#'
#' @examples
#' if(FALSE){
#'
#'   df <- data.frame(x = c(1,2,3), y = c(4,5,6))
#'
#'   column_chart(df, x = df$x, series = c("y")) %>% show()
#' }
#'
#' @importFrom rsvg rsvg
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
           x_margin = get_margins()$left + 50,
           y_vector = NULL,
           y_margin = get_margins()$top + 50) {
    if (!is.null(x_vector) & !is.null(bar_width)) {
      width <- length(x_vector) * 1.5 * bar_width + x_margin
    }
    if (!is.null(y_vector) & ! is.null(bar_width)) {
      height <- length(y_vector) * 1.5 * bar_width + y_margin
    }
    svg_string <- paste(
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
    return(svg_string)
  }


finalize <- function(svg_string) {
  return(paste(svg_string, '</svg>', sep = "\n"))
}


#' Add IBCS compliant legend.
#'
#' @param svg_string one element character vector containing SVG graphic statements. Legend will be added to this plot.
#' @param line1 first line of title. Element(s) of the structure dimension represent the object of the report, typically a legal entity, an organization unit, or a line of business
#' @param line2_measure First part of second line of the title. It will be in bold text. It should represent business measure being analyzed.
#' @param line2_rest Second part of second line of the title. It should represent units of measure.
#' @param line3 Third line of the title, it should indicate time, scenarios, variances, etc
#'
#' @inherit bar_chart return
#' @export
#'
#' @examples
#' df <- data.frame(x = 2010:2015, sales = rnorm(6,10, 2))
#' column_chart(df, df$x, 'sales') %>%
#'   add_title(line1 = 'Department of Big Computers',
#'     line2_measure = "Sales",
#'     line2_rest = "in mEUR",
#'     line3 = "2010..2015")
#'
add_title <- function(svg_string, line1, line2_measure, line2_rest, line3=""){
  size = get_svg_size(svg_string)
  svg_string <- initialize(svg_string_append = svg_string, width = size[1], height = size[2]) %>%
    draw_text(text = line1, x = 0, y = 12, text_anchor = "start") %>%
    draw_text(text = line2_measure, x = 0, y = 24, text_anchor = "start", text_weight = "bold") %>%
    draw_text(text = line2_rest, x = str_width(line2_measure, bold = T)+2, y = 24, text_anchor = "start") %>%
    draw_text(text = line3, x = 0, y = 36, text_anchor = "start") %>%
    finalize()
  class(svg_string) <- c('tidychart', 'character')
  return(svg_string)
}

get_svg_size <- function(svg_string){
  size <- numeric()
  size[1] <- stringr::str_extract(svg_string, 'width="\\d+\\.?\\d*"') %>% stringr::str_extract("\\d+\\.?\\d*") %>% as.numeric()
  size[2] <- stringr::str_extract(svg_string, 'height="\\d+\\.?\\d*"') %>% stringr::str_extract("\\d+\\.?\\d*") %>% as.numeric()
  return(size)
}

#' Save svg image.
#'
#' @param svg_string string containing SVG statements
#' @param path path to file where image will be saved
#'
#' @return No return value, called for side effects
#' @export
#'
SVGsave <- function(svg_string, path){
  write(svg_string, path)
}


# https://www.r-bloggers.com/2016/07/round-values-while-preserve-their-rounded-sum-in-r/
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}
