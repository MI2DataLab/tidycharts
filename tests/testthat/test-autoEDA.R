context('Auto EDA')

test_that('histogram breaks work',{
  expect_equal(
    get_hist_breaks(hist(mtcars$mpg, plot = FALSE)),
    c('10-15', '15-20', '20-25', '25-30', '30-35'))
})

test_that('histo_chart works',{
  expect_magick(histo_chart(iris$Sepal.Length) %>% show())
})


test_that('auto histogram works',{
  expect_magick(auto_histogram(mtcars) %>% show())
})
