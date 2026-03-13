# brokenstick: A package for irregular longitudinal data.

The broken stick model describes a set of individual curves by a linear
mixed model using second-order linear B-splines. The main use of the
model is to align irregularly observed data to a user-specified grid of
break ages.

## Details

The brokenstick package contains functions for fitting a broken stick
model to data, for predicting broken stick curves for new data, and for
plotting the results.

## Note

This work was supported by the Bill & Melinda Gates Foundation. The
contents are the sole responsibility of the authors and may not
necessarily represent the official views of the Bill & Melinda Gates
Foundation or other agencies that may have supported the primary data
studies used in the present study.

## brokenstick functions

The main functions are:

|  |  |
|----|----|
| [`brokenstick()`](https://growthcharts.org/brokenstick/reference/brokenstick.md) | Fit a broken stick model to irregular data |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | Plot observed and fitted trajectories by group |
| [`predict()`](https://rdrr.io/r/stats/predict.html) | Obtain predictions on new data |
| [`summary()`](https://rdrr.io/r/base/summary.html) | Extract object summaries |

The following functions are user-oriented helpers:

|  |  |
|----|----|
| [`coef()`](https://rdrr.io/r/stats/coef.html) | Extract estimated parameters |
| [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) | Calculate fitted values |
| [`get_knots()`](https://growthcharts.org/brokenstick/reference/get_knots.md) | Obtain the knots from a broken stick model |
| [`get_omega()`](https://growthcharts.org/brokenstick/reference/get_omega.md) | Extract variance-covariance of random effects |
| [`get_r2()`](https://growthcharts.org/brokenstick/reference/get_r2.md) | Obtain proportion of explained variance |
| [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) | Extract model frame |
| [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) | Extract design matrix |
| [`residuals()`](https://rdrr.io/r/stats/residuals.html) | Extract residuals from broken stick model |

The following functions perform calculations:

|  |  |
|----|----|
| [`set_control()`](https://growthcharts.org/brokenstick/reference/set_control.md) | Set controls to steer calculations |
| [`control_kr()`](https://growthcharts.org/brokenstick/reference/control_kr.md) | Set controls for the `kr` method |

## References

van Buuren, S. (2023). Broken Stick Model for Irregular Longitudinal
Data. *Journal of Statistical Software*, 106(7), 1–51.
<doi:10.18637/jss.v106.i07>

van Buuren, S. (2018). *Flexible Imputation of Missing Data. Second
Edition*. Chapman & Hall/CRC. Chapter 11.
<https://stefvanbuuren.name/fimd/sec-rastering.html#sec:brokenstick> \#'
@keywords internal

## See also

[`brokenstick`](https://growthcharts.org/brokenstick/reference/brokenstick.md),
[`EB`](https://growthcharts.org/brokenstick/reference/EB.md),
[`predict.brokenstick`](https://growthcharts.org/brokenstick/reference/predict.md)

## Author

**Maintainer**: Stef van Buuren <stef.vanbuuren@tno.nl>
