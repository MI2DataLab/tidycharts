#' Function to render SVG image as htmlwidget
#'
#' Use this function to show SVG images from given string in SVG embedded in HTML.
#'
#' @param svg_string one element character vector with image in svg format
#' @param width width of the widget
#' @param height height of the widget
#' @param elementId  HTML element ID
#'
#' @import htmlwidgets
#' @return No return value, called for side effects
#'
#' @export
SVGrenderer <- function(svg_string, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    message = svg_string
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'SVGrenderer',
    x,
    width = width,
    height = height,
    package = 'tidycharts',
    elementId = elementId
  )
}
