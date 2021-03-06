# brokenstick 1.1.1

- Handles an edge case that crashed `predict()`

# brokenstick 1.1.0

This version adds a couple of minor alterations.

- Updates cran-comments
- Adds a link to the JSS manuscript in the `description` field
- Removes unnecessary `\dontrun{}` directives
- Exports `parse_formula()` to remove `:::` from examples
- Sanitises chunk names by removing `:` and `_` characters
- Corrects some "first-order" mindo's to "second-order"
- Repairs plotting glitch in `oldfriends.Rmd`
- Limits number of printed rows in `predict.brokenstick()` example

# brokenstick 1.0.0

- Ready for CRAN --> Move up to version 1.0.0
- The package is now hosted on `https://github.com/growthcharts/brokenstick/`

# brokenstick 0.78.0

- Prepare package for first CRAN submission

# brokenstick 0.77.0

- Add documentation for the brokenstick class object
- Add JSS manuscript as a vignette
- Remove the prediction vignette and its dependencies
- Extend `plot.brokenstick()` with the ability to plot imputed trajectories
- Add the `weightloss` data
- Fail early when user specifies `degree > 1`

# brokenstick 0.76.0

- Adds argument `what` to `plot.brokenstick()`
- Solves a bug that always yielded zero rows for case 3 prediction
- Solves a data combination problem in `predict()` when the group variable is a factor
- Add a better explanation of the `boundary` parameter
- Evades that `model.matrix()` removes rows with `NA` if `degree = 0`

# brokenstick 0.75.0

- This version trims down the package in various ways
- Removes dependencies of `hardhat` and `recipes`
- Makes the `brokenstick` object smaller since no blueprints are stored
- Removes the `recipe` interface to the `brokenstick()` function
- Moves `ggplot2` to `suggests`
- Copies over the `install.on.demand()` function from `mice`

# brokenstick 0.72.1

- Imports `recipes::recipe()` to inform R package installation process  

# brokenstick 0.72.0

- Adds badges, resources and references to README
- Updates license
- Updates CITATION

# brokenstick 0.71.0

- Removes the dependency on `growthstandards`
- Updates and corrects `plot` examples 
- Tries to evade `ggplot2` out-of-range/missing messages through better filtering

# brokenstick 0.70.1

- Adds support for brokenstick model with `degree = 0`

# brokenstick 0.70.0

* This version jump illustrates big and breaking changes:

1. `brokenstick` adopted the `tidymodels` philosophy, and now includes
a dependency on `hardhat`. It is now possible to fit a model using 
five different interfaces. There is no need anymore the hardcode variable
names in the source data.

2. This version introduces a new estimation method, the Kasim-Raudenbush
sampler. The new method is more flexible and faster than `lme4::lmer()`
when the number of knots is large.

3. This version introduces two simple correlation models that may be 
used to smooth out the variance-covariance matrix of the random effects.

4. The definition of the `brokenstick` class has changed. Objects of 
class `brokenstick` do no longer store the training data. 

5. The `brokenstick_export` class is retired.

6. The `predict()` function is fully rewritten as has now a new interface.
Since the `brokenstick` class does not store the training data anymore, 
the `predict()` function now obtains a `new_data` argument. Syntax that 
worked for `brokenstick` package before `0.70.0` does not work anymore
and should be updated. The `shape` argument replaces the `output` 
argument.

7. The `plot()` function is rewritten, and now requires a `new_data` 
specification.

8. Retired functions: `brokenstick()` replaces `fit_brokenstick()`, 
`predict.brokenstick()` replaces `predict.brokenstick_export()`, 
`get_r2()` replaces `get_pev()`

9. Removed functions: `get_data()`, `get_X()`, `export()`

# brokenstick 0.62.0

* This version simplifies the plotting functions
* Renders `ggplot` objects sharper in vignettes by `svglite`
* Drops the `pkg` argument in `plot.brokenstick()`
* Lessens the dependency on `rbokeh`
* Replaces `hbgd` (which is no longer developed) by `growthstandards` package
* Replaces `smocc_50`/`fit_50` by `smocc_200`/`fit_200` 
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

The broken stick model describes a set of individual curves by a linear mixed model using second-order linear B-splines. The model can be used

- to smooth growth curves by a series of connected straight lines;
- to align irregularly observed curves to a common age grid;
- to create synthetic curves at a user-specified set of break ages;
- to estimate the time-to-time correlation matrix;
- to predict future observations.

The user specifies a set of break ages at which the straight lines connect. Each individual obtains an estimate at each break age, so the set of estimates of the individual form a smoothed version of the observed trajectory.

The main assumptions of the broken stick model are that the development between the break ages follows a straight line, and that the broken stick estimates follow a common multivariate normal distribution. In order to conform to the assumption of multivariate normality, the user may fit the broken stick model on suitably transformed data that yield the standard normal (Z-score) scale.

This lecture outlines the model and introduces the brokenstick R package.

