context("Joining plots (facetting)")

test_that("Simple joining works",{
  set.seed(123)
  df <- data.frame(
    mon = month.abb[1:6],
    values = rnorm(6)
  )
  expect_magick(
    join_plots(
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values')
    ) %>% show()
  )
})



test_that("Simple 2x2 joining works",{
  set.seed(123)
  df <- data.frame(
    mon = month.abb[1:6],
    values = rnorm(6)
  )
  expect_magick(
    join_plots(
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      nrows = 2,
      ncols = 2
    ) %>% show()
  )
})


test_that("Simple 3x2 joining works",{
  set.seed(123)
  df <- data.frame(
    mon = month.abb[1:6],
    values = rnorm(6)
  )
  expect_magick(
    join_plots(
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      column_chart(df, x = 'mon', series = 'values'),
      nrows = 3,
      ncols = 2
    ) %>% show()
  )
})
