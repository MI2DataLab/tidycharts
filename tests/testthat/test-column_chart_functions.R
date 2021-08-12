context('Column charts')

library(datasets)
library(dplyr)
data("iris")
df <-
  iris %>% group_by(Species) %>% summarise(avg = mean(-1 * Sepal.Length))

df2 <-
  iris %>% group_by(Species) %>% summarise(
    avg = mean(Sepal.Length),
    std = sqrt(var(Sepal.Length)),
    median = median(Sepal.Length)
  )

df3 <- data.frame('sales' = c(1.5, 2.5, 1, -0.5, 5, 4),
                  'month' = c('a','b','c','d','e', 'F'))

df4 <- data.frame(
  'month' = c('a','b','c','d','e', 'F', 'G'),
  's1' = c(1.5, 1.5, 1.6, 1.7, 1.3, 1.2, 1),
  's2' = c(1.9, 1.8, 1  , 1.3, 1.7, 1.5, 2),
  's3' = c(0.9, 1.8, 1  , 1.3, 1.7, 1.5, 2))

styles <- data.frame(
  's1' = c(rep('actual',6), 'forecast'),
  's2' = rep('plan',7),
  's3' = rep('previous',7)
)

test_that('Simple column charts are generating',{
  expect_magick(
    column_chart(df, x = df$Species, series = c("avg"), styles = c('actual', 'actual', 'forecast')) %>% show()
  )

  # with positive values
  df$avg <- df$avg * -1
  expect_magick(
    column_chart(df, x = df$Species, series = c("avg")) %>% show()
  )

  # with legend
  expect_magick(
    column_chart(df, x = df$Species, series = c("avg")) %>%
    add_title(line1 = 'Iris', line2_measure = "Typical measure", line2_rest = "in cm", line3 = "2020..2021") %>% show()
  )

})
test_that('Most simple example works',{
  df <- data.frame(x = c('a','b','c'),
                   val = c(1,2,3))
  expect_magick(column_chart(df, x='x', series = 'val') %>% show())
})


test_that('Stacked column charts are generating',{
  # negative values
  df2[-1] = df2[-1] * -1
  expect_magick(
    column_chart(
      df2,
      x = df2$Species,
      series = c("avg", "std", "median")
    ) %>% show()
  )


  # mixed positive and negative values
  df2[1, 2:4] <- df2[1, 2:4] * -1
  expect_magick(column_chart(
    df2,
    x = df2$Species,
    series = c("avg", "std", "median")
  ) %>% show())

  df2[1, 2:4] <- df2[1, 2:4] * -1

  # positive values
  df2[-1] = df2[-1] * -1
  expect_magick(column_chart(
    df2,
    x = df2$Species,
    series = c("avg", "std", "median")
  ) %>% show()
  )
})


test_that('Normalized plot are working', {

  # positive values
  expect_magick(column_chart_normalized(
    df2,
    x = df2$Species,
    series = c("avg", "std", "median")
  ) %>% show())

  # negative values
  df2[-1] = df2[-1] * -1
  expect_magick(column_chart_normalized(
    df2,
    x = df2$Species,
    series = c("avg", "std", "median")
  ) %>% show())
  df2[-1] = df2[-1] * -1
})

test_that('Column chart with reference line are working',{

  # column chart referenced
  expect_magick(column_chart_reference(
    df,
    x = df$Species,
    series = c("avg"),
    ref_value = 5.01
  ) %>% show())


  # column chart referenced with styles
  expect_magick(
    column_chart_reference(
      df,
      x = df$Species,
      series = c("avg"),
      ref_value = 5.01,
      styles = c('actual', 'actual', 'plan')
    ) %>% show()
  )
})

test_that("Column waterfall is working", {
  # column waterfall chart
  expect_magick(column_chart_waterfall(df, x = df$Species, series = c("avg")) %>% show())

  # column waterfall chart with styles
  expect_magick(column_chart_waterfall(
    df,
    x = df$Species,
    series = c("avg"),
    styles = c('previous', 'actual', 'actual')
  ) %>% show())

  expect_magick(column_chart_waterfall(df3, x = df3$month, series = c('sales')) %>% show())
})

test_that('Absolute variance plots are working', {
  # absolute variance column chart
  expect_magick(column_chart_absolute_variance(df4$month, df4$s1, df4$s2, colors = 1) %>% show())
})

test_that('Grouped column chart are working', {
  # grouped column chart
  expect_magick(
    column_chart_grouped(
      df4$month,
      foreground = df4$s1,
      background = df4$s2,
      markers = df4$s3,
      series_labels = c("Actual", "Looooooooong series name", "Previous year"),
      styles = styles
    ) %>% show()
  )

  # grouped column chart without markers
  expect_magick(
    column_chart_grouped(
      df4$month,
      foreground = df4$s1,
      background = df4$s2,
      series_labels = c("AC", "PY")
    ) %>% show()
  )
})


test_that('Relative variance charts are working', {
  # relative variance column chart
  expect_magick(
    column_chart_relative_variance(
      df4$month,
      df4$s1,
      df4$s2,
      colors = 1,
      x_title = "Serie",
      styles = styles$s1
    ) %>% show()
  )
  # relative variance column chart patological title
  expect_magick(
    column_chart_relative_variance(
      df4$month,
      df4$s1,
      df4$s2,
      colors = 1,
      x_title = "LOOOOOOOOOOOOOOOOOONG TITLE"
    ) %>% show()
  )
})


test_that('Waterfall variance chart is working', {
  # waterfall variance
  expect_magick(
    column_chart_waterfall_variance(
      df4$month,
      df4$s1,
      df4$s2,
      colors = 1,
      result_title = "Total"
    ) %>% show()
  )
})
