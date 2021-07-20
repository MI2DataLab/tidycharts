context("Test horizontal barchart plots")



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
  services = c(621, 545, -302, -44, 39, 30, 34, 333)
)
groups <- c("products")
styles <- c(rep('actual', 6), 'forecast', 'actual')

df <- data.frame(
  animal = c("cat", "doggo", "rabbito"),
  hungry = c(7, 5, 9),
  relaxed = c(3, 4, 5),
  wounded = c(1, 8, 5)
)
srs <- c("hungry", "relaxed", "wounded")

test_that("Basic barchart is working", {
  expect_magick(barchart_plot(data, data$city, groups, groups, styles = styles) %>% show())
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

test_that("", {
  groups2 <- c(groups, 'services')
  expect_magick(barchart_plot_normalized(data, data$city, groups2, groups2) %>% show())
})

test_that("", {
  expect_magick(skip())
})
test_that("", {
  expect_magick(skip())
})


