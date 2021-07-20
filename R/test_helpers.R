# script with custom expect statements for testing

# expect that object is an magick image,
# mainly for simple checking if plots are generating and there are no errors
expect_magick <- function(object){
  act <- quasi_label(rlang::enquo(object), arg = 'object')

  expect(
    is(act$val, 'magick-image'),
    sprintf("%s is class %s, not class 'magick-image'", act$lab, class(act))
  )
  invisible(act$val)
}
