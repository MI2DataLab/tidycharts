context("Joining plots (facetting)")

test_that("Simple joining works",{
  set.seed(123)
  df <- data.frame(
    mon = month.abb[1:6],
    values = rnorm(6)
  )
  expect_magick(
    join_charts(
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
    join_charts(
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
    join_charts(
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

test_that("3x2 joining works with only 5 plots",{
  set.seed(123)
  df <- data.frame(
    mon = month.abb[1:6],
    values = rnorm(6)
  )
  expect_magick(
    join_charts(
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

test_that('facetting works',{
  expect_magick(
    facet_chart(
      data = mtcars,
      facet_by = 'cyl',
      ncols = 3,
      FUN = scatter_plot,
      x = mtcars$hp,
      y = mtcars$qsec,
      legend_title = ''
    ) %>% show()
  )
})

test_that('facetting unexisting column gives error',{
  expect_error(
    facet_chart(
      data = mtcars,
      facet_by = 'other_variable',
      ncols = 3,
      FUN = scatter_plot,
      x = mtcars$hp,
      y = mtcars$qsec,
      legend_title = ''
    ) %>% show()
  )
})

test_that('facetting to 1 column works',{
  expect_magick(
    facet_chart(
      data = mtcars,
      facet_by = 'cyl',
      ncols = 1,
      FUN = scatter_plot,
      x = mtcars$hp,
      y = mtcars$qsec,
      legend_title = ''
    ) %>% show()
  )
})

test_that('facetting to 2 column works',{
  expect_magick(
    facet_chart(
      data = mtcars,
      facet_by = 'cyl',
      ncols = 2,
      FUN = scatter_plot,
      x = mtcars$hp,
      y = mtcars$qsec,
      legend_title = ''
    ) %>% show()
  )
})
