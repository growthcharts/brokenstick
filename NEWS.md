# brokenstick 0.62.0

* This version simplifies the plotting functions
* Imports plotting functions from `ggplot2` only
* Renders `ggplot` objects sharper in vignettes by `svglite`
* Drops the `pkg` argument in `plot.brokenstick()`
* Lessens the dependency on `rbokeh`
* Added a `NEWS.md` file to track changes to the package

# brokenstick 0.61.0

* Added `smocc_50` and `fit_50` demo data
* Removed `smocc.hgtwgt`, `smocc_hgtwgt` and `fit_206 datasets`
* Adapt code and vignettes to reflect replacement of demo data

# brokenstick 0.60.0

* Adapted source to R 4.0.0.

# brokenstick 0.55

* Added new utility function `get_pev()` for proportion explained variance
* `get_knots()` gets a `what` argument
* Now using smarter defaults for `plot()`
* Simplified arguments to `plot()`
* Simplified vignette "Overview of main functions"

# brokenstick 0.54

* Added Support for `ggplot2`
* Made `ggplot2` plot default
* Changed default `show_references` flag to FALSE

# brokenstick 0.53

* This is the version announced during my invited lecture at the 7th Channel Network Conference, Hasselt, Belgium.

Here is the abstract of the lecture:

Broken stick model for individual growth curves

Stef van Buuren

1) Netherlands Organization for Applied Scientific Research TNO
2) Utrecht University

The broken stick model describes a set of individual curves by a linear mixed model using first order linear B-splines. The model can be used

- to smooth growth curves by a series of connected straight lines;
- to align irregularly observed curves to a common age grid;
- to create synthetic curves at a user-specified set of break ages;
- to estimate the time-to-time correlation matrix;
- to predict future observations.

The user specifies a set of break ages at which the straight lines connect. Each individual obtains an estimate at each break age, so the set of estimates of the individual form a smoothed version of the observed trajectory.

The main assumptions of the broken stick model are that the development between the break ages follows a straight line, and that the broken stick estimates follow a common multivariate normal distribution. In order to conform to the assumption of multivariate normality, the user may fit the broken stick model on suitably transformed data that yield the standard normal (Z-score) scale.

This lecture outlines the model and introduces the brokenstick R package.

