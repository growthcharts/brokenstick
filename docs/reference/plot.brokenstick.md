# Plot observed and fitted trajectories by group

The `plot` method for a `brokenstick` object plots the observed and
fitted trajectories of one or more groups.

## Usage

``` r
# S3 method for class 'brokenstick'
plot(x, newdata = NULL, ...)
```

## Arguments

- x:

  An object of class `brokenstick`.

- newdata:

  Optional. A data frame in which to look for variables with which to
  predict. The training data are used if omitted and if `object$light`
  is `FALSE`.

- ...:

  Extra arguments passed down to
  [`predict.brokenstick()`](https://growthcharts.org/brokenstick/reference/predict.md)
  and
  [`plot_trajectory()`](https://growthcharts.org/brokenstick/reference/plot_trajectory.md).

## Value

An object of class
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

By default, `plot(fit)` will plot the observed and fitted data for the
first three groups in the data. The default setting drops the fitted
value at the right boundary knot from the display.

## See also

[predict.brokenstick](https://growthcharts.org/brokenstick/reference/predict.md),
[plot_trajectory](https://growthcharts.org/brokenstick/reference/plot_trajectory.md).

## Author

Stef van Buuren 2023

## Examples

``` r
if (FALSE) { # \dontrun{
# fit model on raw hgt with knots at 0, 1, 2 and 3 years
fit1 <- brokenstick(hgt ~ age | id, smocc_200, knots = 0:2)
gp <- c(10001, 10005, 10022)
plot(fit1, group = gp, xlab = "Age (years)", ylab = "Length (cm)")

# fit model on standard deviation score
fit2 <- brokenstick(hgt_z ~ age | id, smocc_200, knots = 0:2)
plot(fit2, group = gp, xlab = "Age (years)", ylab = "Length (SDS)")

# built-in model with 11 knots
plot(fit_200, group = gp, xlab = "Age (years)", ylab = "Length (SDS)")

# black and white version
plot(fit_200, group = gp, xlab = "Age (years)", ylab = "Length (SDS)",
    color_y = rep("black", 2), shape_y = 1, linetype_y = 3,
    color_yhat = rep("grey20", 2), shape_yhat = NA)
} # }
```
