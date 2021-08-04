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
