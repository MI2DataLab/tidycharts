context("Warnings")


test_that('Function to check if all values have the same sign',
          {
            expect_error(stop_if_pos_neg_values(data.frame(
              s1 = c(1, 2), s2 = c(-1, 2)
            ), series = c('s1', 's2')))
            expect_null(stop_if_pos_neg_values(data.frame(
              s1 = c(1, 2), s2 = c(1, 2)
            ), series = c('s1', 's2')))
          })

test_that('stop_if_many_categories is correct',
          {
            expect_error(stop_if_many_categories(rep('a', 10), max_categories = 5))
            expect_null(stop_if_many_categories('a', max_categories = 5))
          })


test_that('stop_if_color_variance is correct',
          {
            expect_error(stop_if_variance_colors(colors_encoding = 4))
            expect_null(stop_if_variance_colors(colors_encoding = 1))
          })
