#' Explicitly draw tidychart
#'
#' This function overrides default print method for tidycharts and prints them on the
#' viewer pane in RStudio or as output in knitr.
#'
#' @param x object of class tidychart to display
#' @param ... arguments passed to `SVGrenderer` function
#'
#' @return Invisibly returns the object of the tidychart class
#' @export
#'
#' @examples
#' # simply calling a plotting function will result in showing the chart
#' bar_chart(
#'   data = data.frame(
#'     cat = c('a', 'b', 'c'),
#'     series = c(12,15,16)),
#'   cat = 'cat',
#'   series = 'series')
#'
#' # result of the plotting function can also be assigned
#' barchart <- bar_chart(
#'   data = data.frame(
#'     cat = c('a', 'b', 'c'),
#'     series = c(12,15,16)),
#'   cat = 'cat',
#'   series = 'series')
#' # and explicitly printed
#' print(barchart)
#' # or implicitly printed
#' barchart
print.tidychart <- function(x, ...){
  print(SVGrenderer(unclass(x), ...))
  invisible(x)
}


#' Printing in knitr reports
#'
#' Normally you don't want to use this function explicitly.
#' It is called automatically when printing output in knitr.
#'
#' @param x object of class tidychart to display in knitr document
#' @param ... arguments passed to `knit_print` function
#'
#' @return object of class `html_screenshot` or `knit_asis`
#' @export
#'
#' @importFrom knitr knit_print
knit_print.tidychart <- function(x, ...){
  knit_print(SVGrenderer(unclass(x)), ...)
}
