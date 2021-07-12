library(datasets)
library(dplyr)
library(tidyr)

data("iris")

# single series
# with negative values
df <-
  iris %>% group_by(Species) %>% summarise(avg = mean(-1 * Sepal.Length))
column_chart(df, x = df$Species, series = c("avg")) %>% show()

# with positive values
df$avg <- df$avg * -1
column_chart(df, x = df$Species, series = c("avg")) %>% show()


# mulitple series 

df2 <-
  iris %>% group_by(Species) %>% summarise(
    avg = mean(Sepal.Length),
    std = sqrt(var(Sepal.Length)),
    median = median(Sepal.Length)
  )

# negative values
df2[-1] = df2[-1] * -1
column_chart(df2,
             x = df$Species,
             series = c("avg", "std", "median")) %>% show()


# mixed positive and negative values
df2[1,] <- df2[1,] * -1
column_chart(df2,
             x = df$Species,
             series = c("avg", "std", "median")) %>% show()

df2[1,] <- df2[1,] * -1

# positive values
df2[-1] = df2[-1] * -1
column_chart(df2,
             x = df$Species,
             series = c("avg", "std", "median")) %>% show()


# normalized plot
# positive values
column_chart_normalized(df2,
             x = df$Species,
             series = c("avg", "std", "median")) %>% show()

# negative values
df2[-1] = df2[-1] * -1
column_chart_normalized(df2,
                        x = df$Species,
                        series = c("avg", "std", "median")) %>% show()
df2[-1] = df2[-1] * -1

# column chart referenced
column_chart_reference(df, x = df$Species, series = c("avg"), ref_value = 5.01) %>% show()

# column waterfall chart
column_chart_waterfall(df, x = df$Species, series = c("avg")) %>% show()

df3 <- data.frame('sales' = c(1.5, 2.5, 1, -0.5, 5, 4), 
                  'month' = c('a','b','c','d','e', 'F'))
column_chart_waterfall(df3, x = df3$month, series = c('sales')) %>% show()

# absolute variance column chart
df4 <- data.frame(
  'month' = c('a','b','c','d','e', 'F', 'G'),
  's1' = c(1.5, 1.5, 1.6, 1.7, 1.3, 1.2, 1), 
  's2' = c(1.9, 1.8, 1  , 1.3, 1.7, 1.5, 2),
  's3' = c(0.9, 1.8, 1  , 1.3, 1.7, 1.5, 2))
column_chart_absolute_variance(df4$month, df4$s1, df4$s2, colors = 1) %>% show()


# grouped column chart
column_chart_grouped(df4$month, foreground = df4$s1, background = df4$s2, triangles = df4$s3, titles = c("AC", "PY", "PL")) %>% show()

# grouped column chart without triangles
column_chart_grouped(df4$month, foreground = df4$s1, background = df4$s2, titles = c("AC", "PY")) %>% show()

# relative variance column chart
column_chart_relative_variance(df4$month, df4$s1, df4$s2, colors = 1, x_title = "Serie") %>% show()

# waterfall variance
column_chart_waterfall_variance(df4$month, df4$s1, df4$s2, colors = 1, result_title = "Total") %>% show()

# save file :
# write(column_chart_waterfall(df, x = df$Species, series = c("avg")), file = 'svg.svg')

