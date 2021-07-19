context("Styling")

test_that("get_style function works ok", {
  expect_equal(get_style("actual")[['fill']], "rgb(64,64,64)")
})
