context("Scatter and bubble plots")

data <- data.frame(
  x = c(2, 3, 5, 5.5, 7, 9, 2.5, 1, 5, 5.3, 8.5, 6.6),
  value = c(5,3,2,6, 7, 3, 2, 1,7,8,3, 5),
  cat = c("mlem","mlem","mlem","mlem","mlem", "kwa","kwa","kwa", "moo","moo","moo","moo"),
  bubble =c (1,2,3,4,5,4,6,2,1,3, 3.5, 4.5 )
)

data2 <- data.frame(
  x = c(2, 3, 5, 5.5, 7, 9, 2.5, 1, 5, 5.3, 8.5, -15),
  value = c(5,3,2,6, 7, 3, 2, 1,7,8,3, -5),
  cat = c("mlem","mlem","mlem","mlem","mlem", "kwa","kwa","kwa", "moo","moo","moo","moo"),
  bubble =c (1,2,3,4,5,4,6,2,1,3, 3.5, 4.5 )
)

data3 <- data.frame(
  x = c(2, 3, 5, 5.5, 7, 9, 2.5, 1, 5, 5.3, 8.5, -15),
  value = c(5,3,2,6, 7, 3, 2, 1,7,8,3, -5),
  cat = c("mlem","mlem","mlem","mlem","mlem", "kwa","kwa","kwa", "moo","moo","moo","moo"),
  bubble =c (1,2,3,4,5,-4,6,-2,1,-3, 3.5, 4.5 )
)

data4 <- data.frame(
  x = c(2, 3, 5, 5.5, 7, 9, 2.5, 1, 5, 5.3, 8.5, 15),
  value = c(5,3,2,6, 7, 3, 2, 1,7,8,3, 5),
  cat = c("mlem","mlem","mlem","mlem","mlem", "kwa","kwa","kwa", "moo","moo","moo","moo"),
  bubble =c (1,2,3,40,5,4,6, 2,1, 3, 3.5, 4.5 )
)

test_that("Basic scatterplot works without errors", {
  expect_magick(
    scatter_plot(
      data,
      data$x,
      data$value,
      data$cat,
      2,
      1,
      c("time", "in s"),
      c("distance", "in km"),
      "Legenda",
      data$bubble
    ) %>% show()
  )
})

test_that("Scatterplot with negative x and y value works without errors", {
  expect_magick(
    scatter_plot(
      data2,
      data2$x,
      data2$value,
      data2$cat,
      2,
      1,
      c("time", "in s"),
      c("distance", "in km"),
      "Legenda",
      data2$bubble
    ) %>% show()
  )
})

test_that("Simple scatterplot", {
  expect_magick(
    scatter_plot(
      data2,
      data2$x,
      data2$value,
      data2$cat,
      2,
      1,
      c("time", "in s"),
      c("distance", "in km"),
      "Legenda"
    ) %>% show()
  )
})

test_that("Scatterplot with negative bubble value throws error", {
  expect_error(
    scatter_plot(
      data3,
      data3$x,
      data3$value,
      data3$cat,
      2,
      1,
      c("time", "in s"),
      c("distance", "in km"),
      "Legenda",
      data3$bubble
    ) %>% show()
  )
})

test_that("Scatterplot with large difference in bubble size works without errors",
          {
            expect_magick(
              scatter_plot(
                data4,
                data4$x,
                data4$value,
                data4$cat,
                2,
                1,
                c("time", "in s"),
                c("distance", "in km"),
                "Legenda",
                data4$bubble
              ) %>% show()
            )
          })
