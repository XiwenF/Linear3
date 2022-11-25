
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Linear3

<!-- badges: start -->

[![R-CMD-check](https://github.com/XiwenF/Linear3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/XiwenF/Linear3/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/XiwenF/Linear3/branch/master/graph/badge.svg)](https://app.codecov.io/gh/XiwenF/Linear3?branch=master)
<!-- badges: end -->

The goal of Linear3 is to mimic existing R functions closely related to
fitting linear regression models and anova. The imitated functions
include: lm(), summary(), confint(), hatvalues(), rstudent(),
rstandard(), anova(), Anova(Model, Type = “III”), some output can be
used \$ to extract from lm() and summary() such as residuals, fitted
values and etc.

## Installation

You can install the development version of Linear3 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# install.packages("bench")
# install.packages("ggbeeswarm")
devtools::install_github("XiwenF/Linear3",build_vignettes = T)
```

## Features

Linear3::lr() is used to fit simple linear regression or multiple linear
regression. When fitting the model, you can use the option
include.intercept to decide whether to include or exclude the intercept.
The function doesn’t explicitly render any output until you extract the
desired output with the name after \$. To learn more about items that
can be extracted from this function, go through ?lr help page for
details.

Linear3::ANOVA() is used to obtain the analysis of variance table. You
can choose to obtain a sequential sum of squares ANOVA table or a
partial sum of squares ANOVA table. The output is in a data.frame. For
more detailed usage of this function, please refer to the help page:
?ANOVA.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Linear3)
## basic example code
lr(mpg ~ cyl + wt, mtcars)$coefficients ## Fit a model with intercept and extract coefficients
#>           Coefficients
#> intercept    39.686261
#> cyl          -1.507795
#> wt           -3.190972
ANOVA(mpg ~ cyl + wt + qsec, mtcars, type = "Sequential") ## Get sequential sums of squares 
#>           Df           Sum Sq          Mean Sq          F value
#> cyl        1 817.712952354623 817.712952354623 126.773990941625
#> wt         1 117.162268889444 117.162268889444 18.1642327813455
#> qsec       1 10.5673919599594 10.5673919599594 1.63831384687119
#> Residuals 28 180.604574296886 6.45016336774592                 
#>                         Pr(>F)
#> cyl       6.59347372262592e-12
#> wt        0.000207416926559177
#> qsec         0.211060593447255
#> Residuals
```

For a more detailed tutorial and comparison with existing R functions,
please refer to the booklet by following code browseVignettes(package =
‘Linear3’).
