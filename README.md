
<!-- README.md is generated from README.Rmd. Please edit that file -->

# brokenstick

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/brokenstick)](https://cran.r-project.org/package=brokenstick)
[![](https://img.shields.io/badge/github%20version-2.2.0-orange.svg)](https://growthcharts.org/brokenstick/)

<!-- badges: end -->

The broken stick model describes a set of individual curves by a linear
mixed model using second-order linear B-splines. The main use of the
model is to align irregularly observed data to a user-specified grid of
break ages.

All fitting can done in the Z-score scale, so nonlinearities and
irregular data can be treated as separate problems. This package
contains functions for fitting a broken stick model to data, for
exporting the parameters of the model for independent use outside this
package, and for predicting broken stick curves for new data.

## Installation

Install the `brokenstick` package from CRAN as follows:

``` r
install.packages("brokenstick")
```

The latest version (revision branch) can be installed from GitHub as
follows:

``` r
install.packages("remotes")
remotes::install_github("growthcharts/brokenstick")
```

## Overview

The *broken stick model* describes a set of individual curves by a
linear mixed model using linear B-splines. The model can be used

-   to smooth growth curves by a series of connected straight lines;
-   to align irregularly observed curves to a common age grid;
-   to create synthetic curves at a user-specified set of break ages;
-   to estimate the time-to-time correlation matrix;
-   to predict future observations.

The user specifies a set of break ages at which the straight lines
connect. Each individual obtains an estimate at each break age, so the
set of estimates of the individual form a smoothed version of the
observed trajectory.

The main assumptions of the broken stick model are:

-   The trajectory between the break ages follows a straight line, and
    is generally not of particular interest;
-   Broken stick estimates follow a common multivariate normal
    distribution;
-   Missing data are missing at random (MAR);
-   Individuals are exchangeable and uncorrelated.

In order to conform to the assumption of multivariate normality, the
user may fit the broken stick model on suitably transformed data that
yield the standard normal
(![Z](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Z "Z"))
scale. Unique feature of the broken stick model are:

-   *Modular*: Issues related to non-linearity of the growth curves in
    the observed scale can be treated separately, i.e., outside the
    broken stick model;
-   *Local*: A given data point will contribute only to the estimates
    corresponding to the closest break ages;
-   *Exportable*: The broken stick model can be exported and reused for
    prediction for new data in alternative computing environments.

The `brokenstick` package contains functions for

-   Fitting the broken stick model to data,
-   Plotting individual trajectories,
-   Predicting broken stick estimates for new data.

## Resources

### Background

1.  I took the name *broken stick* from Ruppert, Wand, and Carroll
    ([2003](#ref-ruppert2003)), page 59-61, but it is actually much
    older.
2.  As far as I know, de Kroon et al. ([2010](#ref-dekroon2010)) is the
    first publication that uses the broken stick model without the
    intercept in a mixed modelling context. See [The Terneuzen birth
    cohort: BMI changes between 2 and 6 years correlate strongest with
    adult
    overweight](https://stefvanbuuren.name/publications/2010%20TBC%20Overweight%20-%20PLoS%20ONE.pdf).
3.  The model was formally defined and extended in [Flexible Imputation
    of Missing Data (second
    edition)](https://stefvanbuuren.name/fimd/sec-rastering.html#sec:brokenstick).
    See van Buuren ([2018](#ref-vanbuuren2018)).
4.  The evaluation by Anderson et al. ([2019](#ref-anderson2019))
    concluded:

> > We recommend the use of the brokenstick model with standardised
> > Z‐score data. Aside from the accuracy of the fit, another key
> > advantage of the brokenstick model is that it is easier to fit and
> > provides easily interpretable estimates of child growth
> > trajectories.

### Instructive materials

-   [Companion site](https://growthcharts.org/brokenstick/) contains
    vignettes and articles that explain the model and the use of the
    software;
-   Paper in preparation: *Broken Stick Model for Irregular Longitudinal
    Data*:
    [html](https://growthcharts.org/brokenstick/articles/manual/manual.html).

### References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-anderson2019" class="csl-entry">

Anderson, C., R. Hafen, O. Sofrygin, L. Ryan, and HBGDki Community.
2019. “Comparing Predictive Abilities of Longitudinal Child Growth
Models.” *Statistics in Medicine* 38 (19): 3555–70.

</div>

<div id="ref-dekroon2010" class="csl-entry">

de Kroon, M. L. A., C. M. Renders, J. P. van Wouwe, S. van Buuren, and
R. A. Hirasing. 2010. “The Terneuzen Birth Cohort: BMI Changes Between 2
and 6 Years Correlate Strongest with Adult Overweight.” *PloS ONE* 5
(2): e9155.

</div>

<div id="ref-ruppert2003" class="csl-entry">

Ruppert, D., M. P. Wand, and R. J. Carroll. 2003. *Semiparametric
Regression*. Cambridge: Cambridge University Press.

</div>

<div id="ref-vanbuuren2018" class="csl-entry">

van Buuren, S. 2018. *Flexible Imputation of Missing Data. 2nd Edition*.
Boca Raton, FL: CRC Press.

</div>

</div>
