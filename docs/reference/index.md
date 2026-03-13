# Package index

## Main user functions

Functions to fit, predict and visualise the broken stick model.

- [`brokenstick()`](https://growthcharts.org/brokenstick/reference/brokenstick.md)
  :

  Fit a `brokenstick` model to irregular data

- [`coef(`*`<brokenstick>`*`)`](https://growthcharts.org/brokenstick/reference/coef.md)
  : Extract Model Coefficients from brokenstick Object

- [`fitted(`*`<brokenstick>`*`)`](https://growthcharts.org/brokenstick/reference/fitted.brokenstick.md)
  : Calculate fitted values

- [`plot(`*`<brokenstick>`*`)`](https://growthcharts.org/brokenstick/reference/plot.brokenstick.md)
  : Plot observed and fitted trajectories by group

- [`predict(`*`<brokenstick>`*`)`](https://growthcharts.org/brokenstick/reference/predict.md)
  :

  Predict from a `brokenstick` model

- [`print(`*`<brokenstick>`*`)`](https://growthcharts.org/brokenstick/reference/print.md)
  : Print brokenstick object

- [`residuals(`*`<brokenstick>`*`)`](https://growthcharts.org/brokenstick/reference/residuals.brokenstick.md)
  : Extract residuals from brokenstick model

- [`summary(`*`<brokenstick>`*`)`](https://growthcharts.org/brokenstick/reference/summary.md)
  : Create summary of brokenstick object

## Helper functions

Functions to set algorithmic options for model fitting or to extract
components of the fitted object.

- [`control_kr()`](https://growthcharts.org/brokenstick/reference/control_kr.md)
  : Set controls for Kasim-Raudenbush sampler
- [`get_knots()`](https://growthcharts.org/brokenstick/reference/get_knots.md)
  : Obtain the knots from a broken stick model
- [`get_omega()`](https://growthcharts.org/brokenstick/reference/get_omega.md)
  : Extract Variance and Correlation Components
- [`get_r2()`](https://growthcharts.org/brokenstick/reference/get_r2.md)
  : Obtain proportion of explained variance from a broken stick model
- [`parse_formula()`](https://growthcharts.org/brokenstick/reference/parse_formula.md)
  : Parse formula for brokenstick model

## Numerical functions

Functions that perform the necessary calculations for the broken stick
model. Not designed to be called directly.

- [`EB()`](https://growthcharts.org/brokenstick/reference/EB.md) :
  Empirical Bayes predictor for random effects
- [`kr()`](https://growthcharts.org/brokenstick/reference/kr.md) :
  Kasim-Raudenbush sampler for two-level normal model
- [`make_basis()`](https://growthcharts.org/brokenstick/reference/make_basis.md)
  : Create linear splines basis
- [`plot_trajectory()`](https://growthcharts.org/brokenstick/reference/plot_trajectory.md)
  : Plot observed and fitted trajectories from fitted brokenstick model
- [`robust_chol2inv()`](https://growthcharts.org/brokenstick/reference/robust_chol2inv.md)
  : Robust inversion of symmetric matrices
- [`set_control()`](https://growthcharts.org/brokenstick/reference/set_control.md)
  : Set controls to steer calculations

## Class documentation

Package-level documentation.

- [`brokenstick-class`](https://growthcharts.org/brokenstick/reference/brokenstick-class.md)
  :

  Class `brokenstick`

- [`brokenstick-package`](https://growthcharts.org/brokenstick/reference/brokenstick-package.md)
  :

  brokenstick: A package for irregular longitudinal data.

## Data objects

Documentation for built-in data objects

- [`smocc_200`](https://growthcharts.org/brokenstick/reference/smocc_200.md)
  : Infant growth of 0-2 years, SMOCC data extract
- [`fit_200`](https://growthcharts.org/brokenstick/reference/fit_200.md)
  : Broken stick model with nine lines for 200 children
- [`fit_200_light`](https://growthcharts.org/brokenstick/reference/fit_200_light.md)
  : Broken stick model with nine lines for 200 children (light)
- [`weightloss`](https://growthcharts.org/brokenstick/reference/weightloss.md)
  : Weight loss self-measurement data
