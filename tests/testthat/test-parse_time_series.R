context('Time series parsing')

test_that("dummy example works", {
  df <- data.frame(
    x = as.Date(c('2021-01-01', '2021-02-01', '2021-03-01')),
    y = c(10, 12, 15)
  )
  result <- parse_time_series(df, x = 'x', y = 'y')
  expected <- data.frame(
    x = c(3,3,3),
    y = c(10,12,15),
    mon = month.abb[1:3]
  )
  expect_equal(result, expected, tolerance = 0.12)
})
