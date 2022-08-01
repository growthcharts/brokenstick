#' Predict from a `brokenstick` model
#'
#' The predictions from a broken stick model coincide with the
#' group-conditional means of the random effects. This function takes
#' an object of class `brokenstick` and returns predictions
#' in one of several formats. The user can calculate predictions
#' for new persons, i.e., for persons who are not part of
#' the fitted model, through the `x` and `y` arguments.
#'
#' @param object A `brokenstick` object.
#'
#' @param newdata Optional. A data frame in which to look for variables with
#' which to predict. The training data are used if omitted and
#' if `object$light` is `FALSE`.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @param x   Optional. A numeric vector with values of the predictor. It could
#' also be the special keyword `x = "knots"` replaces `x` by the
#' positions of the knots.
#'
#' @param y   Optional. A numeric vector with measurements.
#'
#' @param group A vector with group identifications
#'
#' @param shape A string: `"long"` (default), `"wide"` or `"vector"`
#' specifying the shape of the return value. Note that use of `"wide"`
#' with many unique values in `x` creates an unwieldy, large
#' and sparse matrix.
#'
#' @param include_data A logical indicating whether the observed data
#'  from `object$data` and `newdata` should be included into the
#'  return value. The default is `TRUE`. Use `include_data = FALSE` to
#'  keep only added data points (e.g. knots or observed data specified
#'  by `x` and `y`). Setting `include_data = FALSE` is useful in
#'  combination with `shape = "wide"` to avoid the warning
#'  `Values from '.pred' are not uniquely identified.` For convenience,
#'  in the special case `x = "knots"` the function overwrites
#'  `include_data` to `FALSE` to evade observed ages to show up in the
#'  wide matrix.
#'
#' @param strip_data Deprecated. Use `include_data` instead.
#'
#' @inheritParams get_knots
#'
#' @details
#'
#' The function `predict()` calculates predictions for every row in
#' `newdata`. If the user specifies no `newdata` argument, then the
#' function sets `newdata` equal to the training data (`object$data`
#' if `object$light` is `FALSE`). For a light object without a
#' `newdata` argument, the function throws the warning
#' "Argument 'newdata' is required for a light brokenstick object." and
#' returns `NULL`.
#'
#' It is possible to tailor the behaviour of `predict()` through the
#' `x`, `y` and `group` arguments. What exactly happens depends on
#' which of these arguments is specified:
#'
#' 1. If the user specifies `x`, but no `y` and `group`, the function
#' returns - for every group in `newdata` - predictions at the
#' specified `x` values. This method will use the data from `newdata`.
#' 2. If the user specifies `x` and `y` but no `group`, the function
#' forms a hypothetical new group with the `x` and `y` values. This
#' method uses no information from `newdata`, and also works for
#' a light `brokenstick` object.
#' 3. If the user specifies `group`, but no `x` or `y`, the function
#' searches for the relevant data in `newdata` and limits its
#' predictions to those groups. This is useful if the user needs
#' a prediction for only one or a few groups. This does not work for
#' a light `brokenstick` object.
#' 4. If the user specifies `x` and `group`, but no `y`, the function
#' will create new values for `x` in each `group`, search for the relevant
#' data in `newdata` and provide predictions at values of `x` in those
#' groups.
#' 5. If the user specifies `x`, `y` and `group`, the function
#' assumes that these vectors contain additional data on top on what is
#' already available in `newdata`. The lengths of `x`,
#' `y` and `group` must match.
#' For a light `brokenstick` object, case effectively becomes
#' case 6. See below.
#' 6. As case 5, but now without `newdata` available. All data are
#' specified through `x`, `y` and `group` and form a data frame.
#' Matching to `newdata` is attempted, but as long as group id's are
#' different from the training sample effectively new cases will be
#' made.
#' @return
#' If `shape == "long"` a long `data.frame` of predictions. If `x`, `y` and `group`
#' are not specified, the number of rows in the data frame is guaranteed to
#' be the same as the number of rows in `newdata`.
#'
#' If `shape == "wide"` a wide `data.frame` of predictions, one record per group. Note
#' that this format could be inefficient if observations times vary between
#' subjects.
#'
#' If `shape == "vector"` a vector of predicted values, of all x-values and groups.
#'
#' If the function finds no data, it throws a warnings and returns `NULL`.
#' @examples
#' library("dplyr")
#'
#' # -- Data
#'
#' train <- smocc_200[1:1198, ]
#' test <- smocc_200[1199:1940, ]
#' \dontrun{
#' # -- Fit model
#'
#' fit <- brokenstick(hgt_z ~ age | id, data = train, knots = 0:2, seed = 1)
#' fit_light <- brokenstick(hgt_z ~ age | id,
#'   data = train, knots = 0:2,
#'   light = TRUE, seed = 1
#' )
#'
#' # -- Predict, standard cases
#'
#' # Use train data, return column with predictions
#' pred <- predict(fit)
#' identical(nrow(train), nrow(pred))
#'
#' # Predict without newdata, not possible for light object
#' predict(fit_light)
#'
#' # Use test data
#' pred <- predict(fit, newdata = test)
#' identical(nrow(test), nrow(pred))
#'
#' # Predict, same but using newdata with the light object
#' pred_light <- predict(fit_light, newdata = test)
#' identical(pred, pred_light)
#'
#'
#' # -- Predict, special cases
#'
#'
#' # -- Case 1: x, -y, -group
#'
#' # Case 1: x as "knots", standard estimates, train sample (n = 124)
#' z <- predict(fit, x = "knots", shape = "wide")
#' head(z, 3)
#'
#' # Case 1: x as values, linearly interpolated, train sample (n = 124)
#' z <- predict(fit, x = c(0.5, 1, 1.5), shape = "wide", include_data = FALSE)
#' head(z, 3)
#'
#' # Case 1: x as values, linearly interpolated, test sample (n = 76)
#' z <- predict(fit, test, x = c(0.5, 1, 1.5), shape = "wide", include_data = FALSE)
#' head(z, 3)
#'
#' # Case 1: x, not possible for light object
#' z <- predict(fit_light, x = "knots")
#'
#' # -- Case 2: x, y, -group
#'
#' # Case 2: form one new group with id = 0
#' predict(fit, x = "knots", y = c(1, 1, 0.5, 0), shape = "wide")
#'
#' # Case 2: works also for a light object
#' predict(fit_light, x = "knots", y = c(1, 1, 0.5, 0), shape = "wide")
#'
#'
#' # -- Case 3: -x, -y, group
#'
#' # Case 3: Predict at observed age for subset of groups, training sample
#' pred <- predict(fit, group = c(10001, 10005, 10022))
#' head(pred, 3)
#'
#' # Case 3: Of course, we cannot do this for light objects
#' pred_light <- predict(fit_light, group = c(10001, 10005, 10022))
#'
#' # Case 3: We can use another sample. Note there is no child 999
#' pred <- predict(fit, test, group = c(11045, 11120, 999))
#' tail(pred, 3)
#'
#' # Case 3: Works also for a light object
#' pred_light <- predict(fit_light, test, group = c(11045, 11120, 999))
#' identical(pred, pred_light)
#'
#' # -- Case 4: x, -y, group
#'
#' # Case 4: Predict at specified x, only in selected groups, train sample
#' pred <- predict(fit, x = c(0.5, 1, 1.25), group = c(10001, 10005, 10022),
#'         include_data = FALSE)
#' pred
#'
#' # Case 4: Same, but include observed data and sort
#' pred_all <- predict(fit,
#'   x = c(0.5, 1, 1.25), group = c(10001, 10005, 10022)) %>%
#'   dplyr::arrange(id, age)
#'
#' # Case 4: Applies also to test sample
#' pred <- predict(fit, test, x = c(0.5, 1, 1.25), group = c(11045, 11120, 999),
#'  include_data = FALSE)
#' pred
#'
#' # Case 4: Works also with light object
#' pred_light <- predict(fit_light, test, x = c(0.5, 1, 1.25),
#'   group = c(11045, 11120, 999), include_data = FALSE)
#' identical(pred_light, pred)
#'
#' # -- Case 5: x, y, group
#'
#' # Case 5: Add new data to training sample, and refreshes broken stick
#' # estimate at age x.
#' # Note that novel child (not in train) 999 has one data point
#' predict(fit,
#'   x = c(0.9, 0.9, 0.9), y = c(1, 1, 1),
#'   group = c(10001, 10005, 999), include_data = FALSE)
#'
#' # Case 5: Same, but now for test sample. Novel child 899 has two data points
#' predict(fit, test,
#'   x = c(0.5, 0.9, 0.6, 0.9),
#'   y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899),
#'   include_data = FALSE)
#'
#' # Case 5: Also works for light object
#' predict(fit_light, test,
#'   x = c(0.5, 0.9, 0.6, 0.9),
#'   y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899),
#'   include_data = FALSE)
#'
#'
#' # -- Case 6: As Case 5, but without previous data
#'
#' # Case 6: Same call as last, but now without newdata = test
#' # All children are de facto novel as they do not occur in the training
#' # or test samples.
#' # Note: Predictions for 11045 and 11120 differ from prediction in Case 5.
#' predict(fit,
#'   x = c(0.5, 0.9, 0.6, 0.9),
#'   y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899))
#'
#' # This also work for the light brokenstick object
#' predict(fit_light,
#'   x = c(0.5, 0.9, 0.6, 0.9),
#'   y = c(0, 0.5, 0.5, 0.6), group = c(11045, 11120, 899, 899))
#' }
#' @rdname predict
#' @export
predict.brokenstick <- function(object, newdata = NULL,
                                ...,
                                x = NULL, y = NULL, group = NULL,
                                kset = "all",
                                shape = c("long", "wide", "vector"),
                                include_data = TRUE,
                                strip_data = TRUE) {
  shape <- match.arg(shape)
  if (!missing(strip_data)) {
    warning("Argument 'strip_data' is deprecated; please use 'include_data' instead.",
            call. = FALSE)
    include_data <- !strip_data
  }
  rm(strip_data)

  # handle special case: x = "knots"
  # convenience: overwrite include_data when wide
  if (length(x)) {
    if (!is.na(x[1L]) && x[1L] == "knots") {
      x <- get_knots(object, kset = kset)
      if (shape == "wide") include_data <- FALSE
    }
  }

  # check for light object
  if (is.null(x) && is.null(y) ||
      is.null(x) && !is.null(y) ||
      !is.null(x) && is.null(y)) {
    if (is.null(newdata) && object$light) {
      warning("Argument 'newdata' is required for a light brokenstick object.", call. = FALSE)
      return(NULL)
    }
  }

  # Default case: return prediction for every row in newdata
  # if:
  # 1. the user did not specify y, x and group
  # 2. or, the user specified y but not x
  # Note: Sets NULL newdata to training data in object for non-light object
  if ((is.null(x) && is.null(y) && is.null(group)) || is.null(x) && !is.null(y)) {
    if (is.null(newdata) && !object$light) {
      # use training data
      newdata <- object$data
      reset <- FALSE
    }
    if (!is.null(newdata)) {
      # take specified newdata
      reset <- FALSE
    }
  } else {
    # all other specifications involving x, y and group overwrite newdata
    if (is.null(newdata)) {
      if (object$light) {
        newdata <- data.frame()
      } else {
        newdata <- object$data
      }
    }
    newdata <- append_data(newdata, object[["names"]], x = x, y = y, group = group)
    reset <- TRUE
  }

  if (is.null(x) && is.null(y) && !is.null(group)) {
    # case 3: always include data (else we won't see anything)
    include_data <- TRUE
  }

  x <- as.matrix(newdata[, object$names$x, drop = FALSE])
  y <- as.matrix(newdata[, object$names$y, drop = FALSE])
  g <- as.matrix(newdata[, object$names$g, drop = FALSE])
  p <- predict_brokenstick_bridge(object, x, y, g)

  if (!reset) {
    if (shape == "long") {
      return(p)
    }
    if (shape == "vector") {
      return(pull(p))
    }
    if (shape == "wide") {
      return(bind_cols(newdata, p) %>%
               pivot_wider(
                 id_cols = object$names$g,
                 names_from = object$names$x,
                 values_from = ".pred"
               ))
    }
  }

  ret <- bind_cols(newdata, p)
  if (!include_data) {
    ret <- filter(ret, .data[[".source"]] == "added")
  }

  if (shape == "long") {
    return(ret)
  }
  if (shape == "vector") {
    return(pull(ret, ".pred"))
  }
  if (shape == "wide") {
    return(pivot_wider(ret,
                       id_cols = object$names$g,
                       names_from = object$names$x,
                       values_from = ".pred"
    ))
  }
  stop("Internal error")
}

# ------------------------------------------------------------------------------
# Bridge

predict_brokenstick_bridge <- function(model, x, y, g) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  g <- as.matrix(g)

  yhat <- predict_brokenstick_numeric(model, x, y, g)

  if (nrow(x) != nrow(yhat)) {
    warning("Number of rows differs between between data and prediction.", .call = FALSE)
  }

  return(yhat)
}

# ------------------------------------------------------------------------------
# Implementation

predict_brokenstick_numeric <- function(object, x, y, g) {
  if (object$degree > 1L) stop("Cannot predict for degree > 1")

  X <- make_basis(
    x = x,
    internal = get_knots(object, "internal"),
    boundary = get_knots(object, "boundary"),
    degree = object$degree,
    warn = FALSE
  )

  X_s <- split(as.data.frame(X), f = g)
  X_s <- lapply(X_s, as.matrix)
  y_s <- split(y, f = g)

  blup <- mapply(EB,
                 y = y_s,
                 X = X_s,
                 MoreArgs = list(model = object),
                 SIMPLIFY = TRUE
  )
  if (!length(blup)) {
    return(data.frame(.pred = rep(NA_real_, length(x))))
  }

  xv <- get_knots(object, kset = "all")
  if (object$degree == 0L) xv <- xv[-length(xv)]
  long1 <- data.frame(
    group = rep(colnames(blup), each = nrow(blup)),
    x = xv,
    y = NA,
    yhat = as.vector(blup),
    knot = TRUE
  )
  long2 <- data.frame(
    group = as.character(as.vector(g)),
    x = as.vector(x),
    y = as.vector(y),
    yhat = NA,
    knot = FALSE
  )
  long <- bind_rows(long1, long2)

  approx_method <- ifelse(object$degree, "linear", "constant")
  pred <- long %>%
    group_by(.data$group) %>%
    mutate(yhat = approx(
      x = .data$x,
      y = .data$yhat,
      xout = .data$x,
      method = approx_method
    )$y) %>%
    ungroup() %>%
    filter(!.data$knot) %>%
    pull("yhat")

  return(data.frame(.pred = pred))
}
