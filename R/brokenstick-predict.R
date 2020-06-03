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
#' @export
predict_new <- function(object, new_data, type = "numeric", ...) {
  UseMethod("predict_new")
}

#' @rdname predict_new
#' @export
predict_new.brokenstick <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_predict_types())
  predict_brokenstick_bridge(type, object, forged$predictors)
}

valid_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_brokenstick_bridge <- function(type, model, predictors) {
  predictors <- as.matrix(predictors)

  predict_function <- get_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
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
