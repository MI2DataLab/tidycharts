context("Utils/env functions")

test_that('SVGrenderer works', {
  expect_is(initialize() %>%
              finalize() %>%
              SVGrenderer(),
            "htmlwidget")
})



test_that("get_style function works ok", {
  expect_equal(get_style("actual")[['fill']], "rgb(64,64,64)")
})


test_that('Setting colors works', {
  expect_error(set_colors(data.frame(
    'bar_colors' = rep('white', 5),
    'text_colors' = rep('black', 3)
  )))

  expect_error(set_colors(data.frame(
    'bar_colors' = rep('white', 6),
    'other column name' = rep('black', 6)
  )))

  expect_silent(set_colors(data.frame(
    'bar_colors' = rep('white', 6),
    'text_colors' = rep('black', 6)
  )))
})

test_that('Setting styles works',{
  expect_error(set_styles(
    data.frame(
      'bleble' = rep('white', 6),
      'oink' = rep('black',6)
    )
  ))
  expect_silent(set_styles(
    data.frame(
      'stroke' = rep('white', 6),
      'fill' = rep('black',6)
    )
  ))
})

test_that('Setting margins is correct',{
  expect_error(set_margins(list(not_existing_margin_name = 133)))

  expect_error(set_margins(list(top = 'abc')))

  expect_equal(get_margins()$top, 75)

  set_margins(top = 80)
  expect_equal(get_margins()$top, 80)
  set_margins(top = 75)
  expect_equal(get_margins()$top, 75)

})
