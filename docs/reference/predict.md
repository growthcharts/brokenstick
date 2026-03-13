# Predict from a `brokenstick` model

The predictions from a broken stick model coincide with the
group-conditional means of the random effects. This function takes an
object of class `brokenstick` and returns predictions in one of several
formats. The user can calculate predictions for new persons, i.e., for
persons who are not part of the fitted model, through the `x` and `y`
arguments.

## Usage

``` r
# S3 method for class 'brokenstick'
predict(
  object,
  newdata = NULL,
  ...,
  x = NULL,
  y = NULL,
  group = NULL,
  hide = c("right", "left", "boundary", "internal", "none"),
  shape = c("long", "wide", "vector"),
  include_data = TRUE,
  strip_data = TRUE,
  whatknots = "all"
)
```

## Arguments

- object:

  A `brokenstick` object.

- newdata:

  Optional. A data frame in which to look for variables with which to
  predict. The training data are used if omitted and if `object$light`
  is `FALSE`.

- ...:

  Not used, but required for extensibility.

- x:

  Optional. A numeric vector with values of the predictor. It could also
  be the special keyword `x = "knots"` replaces `x` by the positions of
  the knots.

- y:

  Optional. A numeric vector with measurements.

- group:

  A vector with group identifications

- hide:

  Should output for knots be hidden in get, print, summary and plot
  functions? Can be `"left"`, `"right"`, `"boundary"`, `"internal"` or
  `"none"`. The default is `"right"`.

- shape:

  A string: `"long"` (default), `"wide"` or `"vector"` specifying the
  shape of the return value. Note that use of `"wide"` with many unique
  values in `x` creates an unwieldy, large and sparse matrix.

- include_data:

  A logical indicating whether the observed data from `object$data` and
  `newdata` should be included into the return value. The default is
  `TRUE`. Use `include_data = FALSE` to keep only added data points
  (e.g. knots or observed data specified by `x` and `y`). Setting
  `include_data = FALSE` is useful in combination with `shape = "wide"`
  to avoid the warning
  `Values from '.pred' are not uniquely identified.` For convenience, in
  the special case `x = "knots"` the function overwrites `include_data`
  to `FALSE` to evade observed ages to show up in the wide matrix.

- strip_data:

  Deprecated. Use `include_data` instead.

- whatknots:

  Deprecated. Use `hide` instead.

## Value

If `shape == "long"` a long `data.frame` of predictions. If `x`, `y` and
`group` are not specified, the number of rows in the data frame is
guaranteed to be the same as the number of rows in `newdata`.

If `shape == "wide"` a wide `data.frame` of predictions, one record per
group. Note that this format could be inefficient if observations times
vary between subjects.

If `shape == "vector"` a vector of predicted values, of all x-values and
groups.

If the function finds no data, it throws a warnings and returns `NULL`.

## Details

The function [`predict()`](https://rdrr.io/r/stats/predict.html)
calculates predictions for every row in `newdata`. If the user specifies
no `newdata` argument, then the function sets `newdata` equal to the
training data (`object$data` if `object$light` is `FALSE`). For a light
object without a `newdata` argument, the function throws the warning
"Argument 'newdata' is required for a light brokenstick object." and
returns `NULL`.

It is possible to tailor the behaviour of
[`predict()`](https://rdrr.io/r/stats/predict.html) through the `x`, `y`
and `group` arguments. What exactly happens depends on which of these
arguments is specified:

1.  If the user specifies `x`, but no `y` and `group`, the function
    returns - for every group in `newdata` - predictions at the
    specified `x` values. This method will use the data from `newdata`.

2.  If the user specifies `x` and `y` but no `group`, the function forms
    a hypothetical new group with the `x` and `y` values. This method
    uses no information from `newdata`, and also works for a light
    `brokenstick` object.

3.  If the user specifies `group`, but no `x` or `y`, the function
    searches for the relevant data in `newdata` and limits its
    predictions to those groups. This is useful if the user needs a
    prediction for only one or a few groups. This does not work for a
    light `brokenstick` object.

4.  If the user specifies `x` and `group`, but no `y`, the function will
    create new values for `x` in each `group`, search for the relevant
    data in `newdata` and provide predictions at values of `x` in those
    groups.

5.  If the user specifies `x`, `y` and `group`, the function assumes
    that these vectors contain additional data on top on what is already
    available in `newdata`. The lengths of `x`, `y` and `group` must
    match. For a light `brokenstick` object, case effectively becomes
    case 6. See below.

6.  As case 5, but now without `newdata` available. All data are
    specified through `x`, `y` and `group` and form a data frame.
    Matching to `newdata` is attempted, but as long as group id's are
    different from the training sample effectively new cases will be
    made.

## Examples

``` r
library("dplyr")
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# -- Data

train <- smocc_200[1:1198, ]
test <- smocc_200[1199:1940, ]
if (FALSE) { # \dontrun{
# -- Fit model

fit <- brokenstick(hgt_z ~ age | id, data = train, knots = 0:2, seed = 1)
fit_light <- brokenstick(hgt_z ~ age | id,
  data = train, knots = 0:2,
  light = TRUE, seed = 1
)

# -- Predict, standard cases

# Use train data, return column with predictions
pred <- predict(fit)
identical(nrow(train), nrow(pred))

# Predict without newdata, not possible for light object
predict(fit_light)

# Use test data
pred <- predict(fit, newdata = test)
identical(nrow(test), nrow(pred))

# Predict, same but using newdata with the light object
pred_light <- predict(fit_light, newdata = test)
identical(pred, pred_light)


# -- Predict, special cases


# -- Case 1: x, -y, -group

# Case 1: x as "knots", standard estimates, train sample (n = 124)
z <- predict(fit, x = "knots", shape = "wide")
head(z, 3)

# Case 1: x as values, linearly interpolated, train sample (n = 124)
z <- predict(fit, x = c(0.5, 1, 1.5), shape = "wide", include_data = FALSE)
head(z, 3)

# Case 1: x as values, linearly interpolated, test sample (n = 76)
z <- predict(fit, test, x = c(0.5, 1, 1.5), shape = "wide", include_data = FALSE)
head(z, 3)

# Case 1: x, not possible for light object
z <- predict(fit_light, x = "knots")

# -- Case 2: x, y, -group

# Case 2: form one new group with id = 0
predict(fit, x = "knots", y = c(1, 1, 0.5, 0), shape = "wide")

# Case 2: works also for a light object
predict(fit_light, x = "knots", y = c(1, 1, 0.5, 0), shape = "wide")


# -- Case 3: -x, -y, group

# Case 3: Predict at observed age for subset of groups, training sample
pred <- predict(fit, group = c(10001, 10005, 10022))
head(pred, 3)

# Case 3: Of course, we cannot do this for light objects
pred_light <- predict(fit_light, group = c(10001, 10005, 10022))

# Case 3: We can use another sample. Note there is no child 999
pred <- predict(fit, test, group = c(11045, 11120, 999))
tail(pred, 3)

# Case 3: Works also for a light object
pred_light <- predict(fit_light, test, group = c(11045, 11120, 999))
identical(pred, pred_light)

# -- Case 4: x, -y, group

# Case 4: Predict at specified x, only in selected groups, train sample
pred <- predict(fit, x = c(0.5, 1, 1.25), group = c(10001, 10005, 10022),
        include_data = FALSE)
pred

# Case 4: Same, but include observed data and sort
pred_all <- predict(fit,
  x = c(0.5, 1, 1.25), group = c(10001, 10005, 10022)) %>%
  dplyr::arrange(id, age)

# Case 4: Applies also to test sample
pred <- predict(fit, test, x = c(0.5, 1, 1.25), group = c(11045, 11120, 999),
 include_data = FALSE)
pred

# Case 4: Works also with light object
pred_light <- predict(fit_light, test, x = c(0.5, 1, 1.25),
  group = c(11045, 11120, 999), include_data = FALSE)
identical(pred_light, pred)

# -- Case 5: x, y, group

# Case 5: Add new data to training sample, and refreshes broken stick
# estimate at age x.
# Note that novel child (not in train) 999 has one data point
predict(fit,
  x = c(0.9, 0.9, 0.9), y = c(1, 1, 1),
  group = c(10001, 10005, 999), include_data = FALSE)

# Case 5: Same, but now for test sample. Novel child 899 has two data points
predict(fit, test,
  x = c(0.5, 0.9, 0.6, 0.9),
  y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899),
  include_data = FALSE)

# Case 5: Also works for light object
predict(fit_light, test,
  x = c(0.5, 0.9, 0.6, 0.9),
  y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899),
  include_data = FALSE)


# -- Case 6: As Case 5, but without previous data

# Case 6: Same call as last, but now without newdata = test
# All children are de facto novel as they do not occur in the training
# or test samples.
# Note: Predictions for 11045 and 11120 differ from prediction in Case 5.
predict(fit,
  x = c(0.5, 0.9, 0.6, 0.9),
  y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899))

# This also work for the light brokenstick object
predict(fit_light,
  x = c(0.5, 0.9, 0.6, 0.9),
  y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899))
} # }
```
