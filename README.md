
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidycharts

<!-- badges: start -->

[![R-CMD-check](https://github.com/MI2DataLab/tidycharts/workflows/R-CMD-check/badge.svg)](https://github.com/MI2DataLab/tidycharts/actions)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/tidycharts)](https://cran.r-project.org/package=tidycharts)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidycharts)](https://cran.r-project.org/package=tidycharts)
[![Codecov test
coverage](https://codecov.io/gh/SawickiBartosz/tidycharts/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SawickiBartosz/tidycharts?branch=main)
<!-- badges: end -->

The goal of tidycharts is to enable R users to create charts inspired by
[International Business Communication Standards
(IBCS)](https://www.ibcs.com/). The plots are generated in SVG format,
so embedding them in HTML documents is straight forward.

## Installation

You can install the released version of tidycharts from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidycharts")
```

Development version from [GitHub](https://github.com/) can be installed
with:

``` r
devtools::install_github("MI2DataLab/tidycharts")
```

## Example

How to create IBCS inspired charts using tidycharts?

``` r
library(tidycharts) # load the package

# create some data to visualize
df <- data.frame(months = month.abb[1:6],
                 values = round(5 + sin(1:6), 2))

# create chart in a form of character vector containing SVG content
column_chart(df, x = 'months', series = 'values')
```

<img src="man/figures/readme-column.png" width="50%" />

You can easily create other type of plots, ie. lineplots:

``` r
line_chart_markers(df, x = df$months, series = 'values', series_labels = 'values')
```

<img src="man/figures/readme-lines.png" width="50%" />
