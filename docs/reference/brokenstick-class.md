# Class `brokenstick`

The main fitting function
[`brokenstick()`](https://growthcharts.org/brokenstick/reference/brokenstick.md)
returns an object of class `brokenstick`. This object collects the
fitted broken stick model.

## Details

The package exports S3 methods for the `brokenstick` class for the
following generic functions:
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html),
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`print()`](https://rdrr.io/r/base/print.html),
[`residuals()`](https://rdrr.io/r/stats/residuals.html) and
[`summary()`](https://rdrr.io/r/base/summary.html).

The package exports the following helper functions for `brokenstick`
objects:
[`get_knots()`](https://growthcharts.org/brokenstick/reference/get_knots.md),
[`get_omega()`](https://growthcharts.org/brokenstick/reference/get_omega.md)
and
[`get_r2()`](https://growthcharts.org/brokenstick/reference/get_r2.md).

A `brokenstick` object is a list with the following named elements:

## Elements

- `call`:

  Call that created the object

- `names`:

  A named list with three elements (`"x"`, `"y"`, `"g"`) providing the
  variable name for time, outcome and subject, respectively.

- `internal`:

  Numeric vector of with internal knots.

- `boundary`:

  Numeric vector of length 2 with the boundary knots.

- `degree`:

  The `degree` of the B-spline. See
  [`splines::bs()`](https://rdrr.io/r/splines/bs.html). Support only the
  values of 0 (step model) or 1 (broken stick model).

- `method`:

  String, either `"kr"` or `"lmer"`, identifying the fitting model.

- `control`:

  List of control options returned by
  [`set_control()`](https://growthcharts.org/brokenstick/reference/set_control.md)
  used to set algorithmic details.

- `beta`:

  Numeric vector with fixed effect estimates.

- `omega`:

  Numeric matrix with variance-covariance estimates of the broken stick
  estimates.

- `sigma2`:

  Numeric scalar with the mean residual variance.

- `sample`:

  A numeric vector with descriptives of the training data.

- `light`:

  Should the returned object be lighter? If `light = TRUE` the returned
  object will contain only the model settings and parameter estimates
  and not store the `sigma2j`, `sample`, `data`, `imp` and `mod`
  elements. The light object can be used to predict broken stick
  estimates for new data, but does not disclose the training data and is
  small.

- `hide`:

  Should the output for boundary knots be hidden? Can be `"right"`,
  `"left"`, `"boundary"`, `"internal"` or `"none"`. The default is
  `"right"`.

- `sigma2j`:

  Numeric vector with estimates of the residual variance per group. Only
  used by method `"kr"`.

- `data`:

  The training data used to fit the model.

- `imp`:

  The imputations generated for the missing outcome data. Only for
  `method = "kr"`.

- `mod`:

  For `method = "kr"`: A named list with four components, each of class
  [coda::mcmc](https://rdrr.io/pkg/coda/man/mcmc.html). For
  `method = "lmer"`: An object of class
  [lme4::merMod](https://rdrr.io/pkg/lme4/man/merMod-class.html).

## References

<doi:10.18637/jss.v106.i07>

## Author

Stef van Buuren 2023
