context("Utils/env functions")

test_that('SVGrenderer works', {
  expect_is(
    initialize() %>%
      finalize() %>%
      SVGrenderer(),
    "htmlwidget")
})
