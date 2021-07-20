context("Line plots")


test_that("Line plot with few points is working", {
  data <- data.frame(
    cat = c("blop", "mlem", "kwak", "beep", "moo"),
    val1 = c(1, 3, 5, 7, 7),
    val2 = c(3, 3, -3. - 5, -4, 3),
    val3 = c(8, 8.5, -8, -9, 9.2)
  )
  groups <- c("val1", "val2", "val3")

  expect_magick(line_plot_index(data, data$cat, groups, c("jeden", "dwa", "trzy"), 7) %>% show())
  expect_magick(line_plot(data, data$cat, groups, c("jeden", "dwa", "trzy")) %>% show())
})
