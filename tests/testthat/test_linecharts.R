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

test_that("Line plot with many points is working", {
  data <- data.frame(
   x = c(5,25,45,65,85, 30,60,90,30,60,90,30,60,90),
   y=c(3,4,3,5,2, 6, 7, 6, 5, 6, 5, 7, 7, 6),
   cat =c("Jan","Jan","Jan","Jan","Jan","Feb", "Feb","Feb","Mar","Mar","Mar", "Apr", "Apr", "Apr")
  )

  df <- data.frame(
   xdf = c(5,25,45,65, 5,25,45,65,30,60,90,30,60,90),
   ydf = c(7,8,4,6,4,5,2,-1, -3,-4, 4 ,5,2,2),
   cat = c("Jan","Jan","Jan","Jan","Feb", "Feb", "Feb","Feb","Mar","Mar","Mar", "Apr", "Apr", "Apr")
  )

  mlem <- data.frame(
   df_num = c(2,2,2,2,2,2,2,2,2,2,2),
   point_cords = c(1,2,3,4,5,6,7,8,9,10,11)
  )

  lista <- list(data, df)
  xes <- c("x", "xdf")
  yes <- c("y","ydf")
  cats <- c("cat", "cat")
  expect_magick(
    line_plot_many_points_complex(lista, xes, yes, cats, c("kwak", "moo"), mlem$df_num, mlem$point_cords) %>% show()
  )
})

test_that("", {

})

test_that("", {

})
