<!-- README.md is generated from README.Rmd. Please edit that file -->
brokenstick
===========

The broken stick model describes a set of individual curves by a linear mixed model using first order linear B-splines. The main use of the model is to align irregularly observed data to a user-specified grid of break ages.

All fitting can done in the Z-score scale, so nonlinearities and irregular data can be treated as separate problems. This package contains functions for fitting a broken stick model to data, for exporting the parameters of the model for independent use outside this package, and for predicting broken stick curves for new data.

Installation
------------

The `brokenstick` package can be installed from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github("stefvanbuuren/brokenstick")
```

There is currently no CRAN version.

Overview
--------

The *broken stick model* describes a set of individual curves by a linear mixed model using first order linear B-splines. The model can be used

-   to smooth growth curves by a series of connected straight lines;
-   to align irregularly observed curves to a common age grid;
-   to create synthetic curves at a user-specified set of break ages;
-   to estimate the time-to-time correlation matrix;
-   to predict future observations.

The user specifies a set of break ages at which the straight lines connect. Each individual obtains an estimate at each break age, so the set of estimates of the individual form a smoothed version of the observed trajectory.

The main assumptions of the broken stick model are:

-   The development between the break ages follows a straight line, and is generally not of particular interest;
-   Broken stick estimates follow a common multivariate normal distribution;

In order to conform to the assumption of multivariate normality, the user may fit the broken stick model on suitably transformed data that yield the standard normal (*Z*) scale. Unique feature of the broken stick model are:

-   *Modular*: Issues related to nonlinearities of the growth curves in the observed scale can be treated separately, i.e., outside the broken stick model;
-   *Local*: A given data point will contribute only to the estimates corresponding to the closest break ages;
-   *Exportable*: The broken stick model can be exported and reused for prediction for new data in alternative computing environments.

The `brokenstick` package contains functions for

-   Fitting the broken stick model to data,
-   Plotting individual trajectories,
-   Predicting broken stick estimates for new data,
-   Exporting the parameters of the model for independent use outside this package.

Main functions
--------------

The main functions in the `brokenstick` package are:

| Function name   | Description                                       |
|-----------------|---------------------------------------------------|
| `brokenstick()` | Fit a broken stick model to irregular data        |
| `plot()`        | Plot observed and fitted trajectories             |
| `predict()`     | Predict broken stick estimates                    |
| `export()`      | Export minimal broken stick model for publication |
