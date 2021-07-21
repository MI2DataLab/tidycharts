context("Horizontal barchart plots")

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
  products = c(538, 250, -75, -301, 227, 90, 40, 269),
  services = c(621, 545, -302, -44, 39, 30, 34, 333),
  triangles = c(600, 600, -302, 600, 600, 30, 600, 600)
)
groups <- c("products")
series <- c("triangles", "products", "services")
styles <- c(rep('actual', 6), 'forecast', 'actual')

df_styles <- data.frame(
  products = c(rep('plan', 8)),
  services = c(rep('actual', 8)),
  triangles = c(rep('plan', 8))
)


df <- data.frame(
  animal = c("cat", "doggo", "rabbito"),
  hungry = c(7, 5, 9),
  relaxed = c(3, 4, 5),
  wounded = c(1, 8, 5)
)
srs <- c("hungry", "relaxed", "wounded")

df_waterfall <- data.frame(
  'category' = c(
    "Sales",
    "Other income",
    "Personnel expenses",
    "Material expenses",
    "Capital expenses",
    "Investment income"
  ),
  'values' = c(12.8, 1.4, -4.2, -8.5, -3.1, 0.6)
)

test_that("Basic barchart is working", {
  expect_magick(barchart_plot(data, data$city, groups, groups, styles = styles) %>% show())
})

test_that('Only positive values on a barchart_index don\'t throw warnings', {
  expect_magick(barchart_plot_index(df, df$animal, series = srs, index_val = 3, series_labels = srs) %>% show())
})

test_that("Barchart with index line is working", {
  expect_magick(
    barchart_plot_index(
      data,
      cat = data$city,
      groups,
      index_val = 602,
      series_labels = groups
    ) %>% show()
  )
})

test_that("Normalized horizontal barcharts work", {
  groups2 <- c(groups, 'services')
  expect_magick(barchart_plot_normalized(data, data$city, groups2, groups2) %>% show())
})

test_that("Grouped horizontal barcharts work", {
  expect_magick(
    barchart_plot_grouped(data, data$city, series, series, df_styles = df_styles) %>% show()
  )
})

test_that("Grouped horizontal barcharts work with only positive velues", {
  expect_magick(
    barchart_plot_grouped(df, df$animal, srs, srs) %>% show()
  )
})

test_that("Waterfall horizontal barchart works", {
  expect_magick(
    barchart_plot_waterfall(
      df_waterfall$category,
      df_waterfall$values,
      add_result = TRUE,
      result_title = "Profit before tax"
    ) %>% show()
  )
})
