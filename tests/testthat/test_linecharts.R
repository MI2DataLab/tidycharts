context("Line plots")


test_that("Line plot with few points is working without errors", {
  data <- data.frame(
    cat = c("blop", "mlem", "kwak", "beep", "moo"),
    val1 = c(1, 3, 5, 7, 7),
    val2 = c(3, 3, -3. - 5, -4, 3),
    val3 = c(8, 8.5, -8, -9, 9.2)
  )
  groups <- c("val1", "val2", "val3")

  expect_magick(line_chart_markers_reference(data, data$cat, groups, c("jeden", "dwa", "trzy"), 7) %>% show())
  expect_magick(line_chart_markers(data, data$cat, groups, c("jeden", "dwa", "trzy")) %>% show())
})

test_that("Line plot with many points is working without errors", {
  data <- data.frame(
    x = c(5, 25, 45, 65, 85, 30, 60, 90, 30, 60, 90, 30, 60, 90),
    y = c(3, 4, 3, 5, 2, 6, 7, 6, 5, 6, 5, 7, 7, 6),
    cat = c(
      "Jan",
      "Jan",
      "Jan",
      "Jan",
      "Jan",
      "Feb",
      "Feb",
      "Feb",
      "Mar",
      "Mar",
      "Mar",
      "Apr",
      "Apr",
      "Apr"
    )
  )

  df <- data.frame(
    xdf = c(5, 25, 45, 65, 5, 25, 45, 65, 30, 60, 90, 30, 60, 90),
    ydf = c(7, 8, 4, 6, 4, 5, 2, -1, -3, -4, 4 , 5, 2, 2),
    cat = c(
      "Jan",
      "Jan",
      "Jan",
      "Jan",
      "Feb",
      "Feb",
      "Feb",
      "Feb",
      "Mar",
      "Mar",
      "Mar",
      "Apr",
      "Apr",
      "Apr"
    )
  )

  mlem <- data.frame(
    df_num = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
    point_cords = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  )

  lista <- list(data, df)
  xes <- c("x", "xdf")
  yes <- c("y", "ydf")
  cats <- c("cat", "cat")
  expect_magick(
    line_chart_dense_custom(
      lista,
      xes,
      yes,
      cats,
      c("kwak", "moo"),
      mlem$df_num,
      mlem$point_cords
    ) %>% show()
  )
})


test_that('Lineplot with positive values does not throw warnings', {
  df <- data.frame(
    animal = c("cat", "doggo", "rabbito"),
    hungry = c(7, 5, 9),
    relaxed = c(3, 4, 5),
    wounded = c(1, 8, 5)
  )
  srs <- c("hungry", "relaxed", "wounded")
  expect_magick(line_chart_markers(df, df$animal, srs, srs) %>% show())
})



test_that('Lineplot with one series works', {
  df <- data.frame(
    animal = c("cat", "doggo", "rabbito"),
    hungry = c(7, 5, 9),
    relaxed = c(3, 4, 5),
    wounded = c(1, 8, 5)
  )
  srs <- c("hungry")
  expect_magick(line_chart_markers(df, df$animal, srs, srs) %>% show())
})

test_that("Normalized line plot is working without errors", {
  data <- data.frame(
    cat = c("blop", "mlem", "kwak", "beep", "moo"),
    val1 = c(8, 8.5, 8, 9, 9.2),
    val2 = c(5, 6, 5, 7, 7),
    val3 = c(3, 3, 3.5, 4, 3)

  )
  groups <- c("val1", "val2", "val3")
  series_labels <- c("speed", "mlemler", "defence")

  expect_magick(line_chart_normalized(data, data$cat, groups, series_labels, c(NA, 1, 1, 1, NA)) %>% show())
})

test_that("Stacked line plots work without errors", {
  data <- data.frame(
    city = c(
      "Berlin",
      "Munich",
      "Cologne",
      "London",
      "Vienna",
      "Paris",
      "Zurich",
      "Rest"
    ),
    value = c(1159, 795, 377, 345, 266, 120, 74, 602),
    products = c(538, 250, 75, 301, 227, 90, 40, 269),
    services = c(621, 545, 302, 44, 39, 30, 34, 333)
  )
  groups <- c("products", "services")
  series_labels <- groups
  expect_magick(line_chart_stacked(data, data$city, groups, series_labels, T) %>% show())

  data <- data.frame(
    cat = c("blop", "mlem", "kwak", "beep", "moo"),
    val1 = c(8, 8.5, 8, 9, 9.2),
    val2 = c(5, 6, 5, 7, 7),
    val3 = c(3, 3, 3.5, 4, 3)

  )
  groups <- c("val1", "val2", "val3")
  labels <- groups
  expect_magick(line_chart_stacked(data, data$cat, groups, labels, c(NA, 1, 1, NA, NA)) %>% show())
})

test_that('Line plots with many points work without errors', {
  data <- data.frame(
   city = c("Berlin", "Munich", "Cologne", "London", "Vienna", "Paris", "Zurich", "Rest"),
   value = c(1159, 795, 377, 345, 266,120,74,602),
   products = c(538, 250, 75, 301,227,90, 40, 269),
   services = c(621,545,302,44,39,30,34,333)
  )
  groups <- c("products", "services")


  df <- data.frame(
   ser_name = c("products","products","products","products","products","products","products","products"),
   point_coordinates = c(1,2,3,4,5,6,7,8)
  )
  series_labels <-groups
  expect_magick(
    line_chart(data, x = data$city, groups, series_labels, df$ser_name, df$point_coordinates) %>% show()
  )
})


test_that('Lineplot complex with with only positive values doesn\'t give warnings',{
  df <- data.frame(
    x = seq.Date(as.Date('2021-01-01'), as.Date('2022-01-01'), length.out = 200),
    y = 5 * sin(seq(from = 0, to = 2*pi, length.out = 200 )) +  rnorm(200, mean = 5, sd = 0.5)
  )
  df <- df[df$x < as.Date('2021-02-28'),]
  l <- df %>% parse_time_series_column(df = ., x = 'x', y = 'y') %>% list()
  expect_magick(
    line_chart_dense_custom(list = l, vector_x = 'x', vector_y = 'y',
                                  vector_cat = 'cat', series_labels = 'test_data',
                                  df_numbers = 1, point_cords = NULL) %>% show())
  })


test_that('Lineplot wrapper works', {
  dates = seq.Date(as.Date('2021-07-01'), as.Date('2021-08-31'), by = 1)
  df <- data.frame(
    dates = dates,
    y = seq(8, 8, along.with = dates) + rnorm(length(dates), sd = 0.5),
    z = seq(6, 6, along.with = dates) + rcauchy(length(dates), scale = 0.5)
  )
  expect_magick(
    line_chart_dense(
      df,
      dates = 'dates',
      series = c('y', 'z'),
      interval = 'weeks'
    ) %>%
      show()
  )
})
