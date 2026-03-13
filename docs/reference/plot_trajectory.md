# Plot observed and fitted trajectories from fitted brokenstick model

Plot observed and fitted trajectories from fitted brokenstick model

## Usage

``` r
plot_trajectory(
  x,
  newdata = NULL,
  hide = c("right", "left", "boundary", "internal", "none"),
  .x = NULL,
  group = NULL,
  color_y = c(grDevices::hcl(240, 100, 40, 0.7), grDevices::hcl(240, 100, 40, 0.8)),
  size_y = 2,
  linetype_y = 1,
  shape_y = 19,
  color_yhat = c(grDevices::hcl(0, 100, 40, 0.7), grDevices::hcl(0, 100, 40, 0.8)),
  size_yhat = 2,
  linetype_yhat = 1,
  shape_yhat = 19,
  color_imp = c("grey80", "grey80"),
  size_imp = 2,
  ncol = 3L,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  show = c(TRUE, TRUE, FALSE),
  n_plot = 3L,
  scales = "fixed",
  theme = ggplot2::theme_light(),
  whatknots = "droplast",
  ...
)
```

## Arguments

- x:

  An object of class `brokenstick`.

- newdata:

  A `data.frame` or `matrix`

- hide:

  Character indicating which knots should

- .x:

  The `x` argument of the
  [`predict.brokenstick()`](https://growthcharts.org/brokenstick/reference/predict.md)
  function.

- group:

  A vector with group identifications

- color_y:

  A character vector with two elements specifying the symbol and line
  color of the measured data points

- size_y:

  Dot size of measured data points

- linetype_y:

  Line type of data points

- shape_y:

  Symbol for data points

- color_yhat:

  A character vector with two elements specifying the symbol and line
  color of the predicted data points

- size_yhat:

  Dot size of predicted data points

- linetype_yhat:

  Line type of predicted data

- shape_yhat:

  Symbol for predicted data

- color_imp:

  A character vector with two elements specifying the symbol and line
  color of the imputed data

- size_imp:

  Dot size of imputed data

- ncol:

  Number of columns in plot

- xlab:

  The label of the x-axis

- ylab:

  The label of the y-axis

- xlim:

  Vector of length 2 with range of x-axis

- ylim:

  Vector of length 2 with range of y-axis

- show:

  A logical vector of length 3. Element 1 specifies whether the observed
  data are plotted, element 2 specifies whether the broken stick are
  plotted, element 3 specifies whether imputations are plotted. The
  default is `c(TRUE, TRUE, FALSE)`.

- n_plot:

  A integer indicating the number of individual plots. The default is 3,
  which plots the trajectories of the first three groups. The `n_plot`
  is a safety measure to prevent unintended plots of the entire data
  set.

- scales:

  Axis scaling, e.g. `"fixed"`, `"free"`, and so on

- theme:

  Plotting theme

- whatknots:

  Deprecated.

- ...:

  Extra arguments passed down to
  [`predict.brokenstick()`](https://growthcharts.org/brokenstick/reference/predict.md).

## Value

An object of class `ggplot`

## See also

[plot.brokenstick](https://growthcharts.org/brokenstick/reference/plot.brokenstick.md)
