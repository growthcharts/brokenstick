#' Predict from a `brokenstick`
#'
#' @param object A `brokenstick` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
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
#' @param strip_data A logical indicating whether the row with the
#'  observed data from `new_data` should be stripped from the
#'  return. The default is `TRUE`. Set to `FALSE` to infer which data
#'  points are extracted from `new_data`.
#'
#' @details
#'
#' By default, `predict()` will find predictions for every row in
#' `new_data`. It is possible to tailor the behavior through the
#' `x`, `y` and `group` arguments. What exactly happens depends on
#' which of these arguments is specified:
#'
#' 1. If the user specifies `x`, but no `y` and `group`, the function
#' returns - for every group in `new_data` - predictions at `x`
#' values. This method will use the data from `new_data`.
#' 2. If the user specifies `x` and `y` but no `group`, the function
#' forms a hypothetical new group with the `x` and `y` values. This
#' method uses no information from `new_data`.
#' 3. If the user specifies `group`, but no `x` or `y`, the function
#' searches for the relevant data in `new_data` and limits its
#' predictions to the specified groups. This is useful if prediction
#' for only one or a few groups is needed.
#' 4. If the user specifies `x` and `group`, but no `y`, the function
#' will create new values for `x` in each group, search for the relevant
#' data in `new_data` and limit prediction to locations `x` in those
#' groups.
#' 5. If the user specifies `x`, `y` and `group`, the functions
#' assumes that these vectors form a data frame. The lengths of `x`,
#' `y` and `group` must be the same. This procedure uses only
#' information from `newdata` for groups with `group` values that match
#' those on `newdata`.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- smocc_200[1:1198, ]
#' test <- smocc_200[1199:1940, ]
#'
#' # Fit
#' fit <- brokenstick(hgt.z ~ age | id, data = train, knots = 0:3)
#'
#' # Predict, with preprocessing
#' predict_new(fit, test)
#'
#' # case 1: x as knots
#' z <- predict_new(fit, test, x = "knots")
#'
#' # case 2: x and y, one group
#' predict_new(fit, test, x = "knots", y = c(1, 1, 0.5, 0))
#'
#' # case 3: only group
#' predict_new(fit, test, group = c(11045, 11120, 999))
#'
#' # case 4:
#' predict_new(fit, test, x = c(0.5, 1, 1.25), group = c(11045, 11120, 999))
#'
#' # case 5:
#' predict_new(fit, test, x = c(0.5, 1, 1.25), y = c(0, 0.5, 1), group = c(11045, 11120, 999))
#'
#' @export
predict_new <- function(object, new_data, type = "numeric", ...) {
  UseMethod("predict_new")
}

#' @rdname predict_new
#' @export
predict_new.brokenstick <- function(object, new_data, type = "numeric",
                                    ...,
                                    x = NULL, y = NULL, group = NULL,
                                    strip_data = TRUE) {

  if (length(x)) {
    if (x[1L] == "knots")
      x <- get_knots(object)
  }

  # Default case:
  # - the user did not specify y, x and group
  # - the user specified y but not x
  # return prediction for every row in new_data
  if ((is.null(x) && is.null(y) && is.null(group))
      || is.null(x) && !is.null(y)) {
    reset <- FALSE

  } else {

    new_data <- reset_data(new_data, object$names, x = x, y = y, group = group)
    reset <- TRUE
  }

  forged <- hardhat::forge(new_data, object$blueprint, outcomes = TRUE)
  rlang::arg_match(type, valid_predict_types())
  p <- predict_brokenstick_bridge(type, object,
                                  forged$predictors,
                                  forged$outcomes,
                                  forged$extras$roles$group)
  if (!reset) return(p)

  ret <- dplyr::bind_cols(new_data, p)
  if (strip_data) {
    ret <- ret %>%
      dplyr::filter(.data[[".source"]] == "added") %>%
      dplyr::select(-".source")
  }
  ret
}

valid_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_brokenstick_bridge <- function(type, model, x, y, z) {
  # x <- as.matrix(x)
  # y <- as.matrix(y)
  # z <- as.matrix(z)

  predict_function <- get_predict_function(type)
  yhat <- predict_function(model, x)

  hardhat::validate_prediction_size(yhat, x)

  yhat
}

get_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_brokenstick_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_brokenstick_numeric <- function(model, predictors) {
  predictions <- rep(1L, times = nrow(predictors))
  hardhat::spruce_numeric(predictions)
}
