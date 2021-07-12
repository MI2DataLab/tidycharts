initialize <- function(svg_string="", transformation = "") {
  return(
    paste(svg_string,
          paste0(
          '<svg  version="1.1"
          baseProfile="full"
          width="500" height="500" transform="',
          transformation,
          '" >'
          ),
          sep = "\n")
  )
}


finalize <- function(svg_string) {
  return(paste(svg_string, '</svg>', sep = "\n"))
}


show <- function(svg_string) {
  magick::image_read_svg(svg_string, width = 500)
}
