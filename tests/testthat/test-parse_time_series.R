context('Time series parsing')

test_that("dummy months example works", {
  df <- data.frame(x = as.Date(c(
    '2021-01-01', '2021-02-01', '2021-03-01'
  )),
  y = c(10, 12, 15))
  result <- parse_time_series_column(df = df, x = 'x', y = 'y')
  expected <- data.frame(
    x = c(3, 3, 3),
    y = c(10, 12, 15),
    cat = paste(month.abb[1:3])
  )
  expect_equal(result, expected, tolerance = 0.12)
})

test_that('parsing into weeks works', {
  df <- data.frame(x = as.Date(c(
    '2021-07-19', '2021-07-18', '2021-07-24'
  )),
  y = c(10, 21, 15))
  result <-
    parse_time_series_column(
      df = df,
      x = 'x',
      y = 'y',
      convert.to = 'weeks'
    )
  expected <- data.frame(
    x = c(2 / 7, 1 / 7, 1) * 100,
    y = c(10, 21, 15),
    cat = c(29, 29, 29)
  )
  expect_equal(result, expected)
})

test_that("wrapping function works", {
  df <- data.frame(
    x = as.Date(c(
      '2021-01-01', '2021-02-01', '2021-03-01'
    )),
    y1 = c(10, 12, 15),
    y2 = c(11, 10, 14),
    y3 = c(12, 12, 12)
  )
  result <-
    parse_time_series(df = df,
                      dates = 'x',
                      series = c('y1', 'y2', 'y3'))

  expected <- list(
    data.frame(
      x = c(3, 3, 3),
      y = c(10, 12, 15),
      cat = paste(month.abb[1:3])
    ),
    data.frame(
      x = c(3, 3, 3),
      y = c(11, 10, 14),
      cat = paste(month.abb[1:3])
    ),
    data.frame(
      x = c(3, 3, 3),
      y = c(12, 12, 12),
      cat = paste(month.abb[1:3])
    )

  )
  expect_equal(result, expected, tolerance = 0.12)
})
