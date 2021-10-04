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
#' which to predict. If omitted and if `isFALSE(object$light)`, the fitted
#' predictors are used.
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
#'  observed data from `newdata` should be stripped from the
#'  return. The default is `TRUE`. Set to `FALSE` to infer which data
#'  points are extracted from `newdata`.
#'
#' @param shape A string: `"long"` (default), `"wide"` or `"vector"`
#' specifying the shape of the return value. Note that use of `"wide"`
#' with many unique values in `x` creates an unwieldy, large
#' and sparse matrix.
#' @details
#'
#' By default, `predict()` calculates predictions for every row in
#' `newdata`. It is possible to tailor the behavior through the
#' `x`, `y` and `group` arguments. What exactly happens depends on
#' which of these arguments is specified:
#'
#' 1. If the user specifies `x`, but no `y` and `group`, the function
#' returns - for every group in `newdata` - predictions at `x`
#' values. This method will use the data from `newdata`.
#' 2. If the user specifies `x` and `y` but no `group`, the function
#' forms a hypothetical new group with the `x` and `y` values. This
#' method uses no information from `newdata`.
#' 3. If the user specifies `group`, but no `x` or `y`, the function
#' searches for the relevant data in `newdata` and limits its
#' predictions to the specified groups. This is useful if prediction
#' for only one or a few groups is needed.
#' 4. If the user specifies `x` and `group`, but no `y`, the function
#' will create new values for `x` in each group, search for the relevant
#' data in `newdata` and limit prediction to locations `x` in those
#' groups.
#' 5. If the user specifies `x`, `y` and `group`, the functions
#' assumes that these vectors form a data frame. The lengths of `x`,
#' `y` and `group` must be the same. This procedure uses only
#' information from `newdata` for groups with `group` values that match
#' those on `newdata`.
#' 6. As case 5, but now without a `newdata` argument. All data are
#' specified through `x`, `y` and `group`. No matching to `newdata`
#' attempted.
#'
#' @return
#'
#' A tibble of predictions. If `x`, `y` and `group` are not specified,
#' the number of rows in the tibble is guaranteed to be the same as
#' the number of rows in `newdata`.
#'
#' @examples
#' train <- smocc_200[1:1198, ]
#' test <- smocc_200[1199:1940, ]
#'
#' # Fit
#' fit <- brokenstick(hgt.z ~ age | id, data = train, knots = 0:3)
#'
#' # Predict, with preprocessing
#' tail(predict(fit, test), 3)
#'
#' # case 1: x as knots
#' z <- predict(fit, test, x = "knots")
#'
#' # case 2: x and y, one new group
#' predict(fit, test, x = "knots", y = c(1, 1, 0.5, 0))
#'
#' # case 2: x and y, one new group, we need not specify newdata
#' predict(fit, x = "knots", y = c(1, 1, 0.5, 0))
#'
#' # case 3: only group
#' predict(fit, test, group = c(11045, 11120, 999))
#'
#' # case 4: predict at x in selected groups
#' predict(fit, test, x = c(0.5, 1, 1.25), group = c(11045, 11120, 999))
#'
#' # case 5: vectorized
#' predict(fit, test, x = c(0.5, 1, 1.25), y = c(0, 0.5, 1), group = c(11045, 11120, 999))
#'
#' # case 6: vectorized, without newdata, results are different for 11045 and 11120
#' predict(fit, x = c(0.5, 1, 1.25), y = c(0, 0.5, 1), group = c(11045, 11120, 999))
#' @rdname predict
#' @export
predict.brokenstick <- function(object, newdata = NULL,
                                ...,
                                x = NULL, y = NULL, group = NULL,
                                strip_data = TRUE,
                                shape = c("long", "wide", "vector")) {
  shape <- match.arg(shape)

  # handle special case: x = "knots"
  if (length(x)) {
    if (!is.na(x[1L]) && x[1L] == "knots")
      x <- get_knots(object, ...)
  }

  if ((is.null(x) && is.null(y) && is.null(group))
      || is.null(x) && !is.null(y)) {
    # Default case: return prediction for every row in newdata
    # - the user did not specify y, x and group
    # - the user specified y but not x
    if (is.null(newdata))
      stop("Expected argument `newdata` not found.", call. = FALSE)
    reset <- FALSE
  } else {
    # all other specifications involving x, y and group overwrite newdata
    newdata <- reset_data(newdata, object$names, x = x, y = y, group = group)
    reset <- TRUE
  }

  if (is.null(x) && is.null(y) && !is.null(group)) {
    # case 3: do not strip the data (else we won't see anything)
    strip_data <- FALSE
  }

  x <- as.matrix(newdata[, object$names$x, drop = FALSE])
  y <- as.matrix(newdata[, object$names$y, drop = FALSE])
  g <- as.matrix(newdata[, object$names$g, drop = FALSE])
  p <- predict_brokenstick_bridge(object, x, y, g)

  if (!reset) {
    if (shape == "long") return(p)
    if (shape == "vector") return(pull(p))
    if (shape == "wide") {
      return(bind_cols(newdata, p) %>%
               pivot_wider(id_cols = object$names$g,
                           names_from = object$names$x,
                           values_from = ".pred"))
    }
  }

  ret <- bind_cols(newdata, p)
  if (strip_data) {
    ret <- filter(ret, .data[[".source"]] == "added")
  }

  if (shape == "long") return(ret)
  if (shape == "vector") return(pull(ret, ".pred"))
  if (shape == "wide") {
    return(pivot_wider(ret, id_cols = object$names$g,
                       names_from = object$names$x,
                       values_from = ".pred"))
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

  if (nrow(x) != nrow(yhat))
    warning("Number of rows differs between between data and prediction.", .call = FALSE)

  return(yhat)
}

# ------------------------------------------------------------------------------
# Implementation

predict_brokenstick_numeric <- function(object, x, y, g) {

  if (object$degree > 1L) stop("Cannot predict for degree > 1")

  X <- make_basis(x = x,
                  knots = get_knots(object, "knots"),
                  boundary = get_knots(object, "boundary"),
                  degree = object$degree,
                  warn = FALSE)

  X_s <- split(as.data.frame(X), f = g)
  X_s <- lapply(X_s, as.matrix)
  y_s <- split(y, f = g)

  blup <- mapply(EB,
                 y = y_s,
                 X = X_s,
                 MoreArgs = list(model = object),
                 SIMPLIFY = TRUE)
  if (!length(blup))
    return(data.frame(.pred = rep(NA_real_, length(x))))

  xv <- get_knots(object)
  if (object$degree == 0L) xv <- xv[-length(xv)]
  long1 <- data.frame(
    group = rep(colnames(blup), each = nrow(blup)),
    x = xv,
    y = NA,
    yhat = as.vector(blup),
    knot = TRUE)
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
    mutate(yhat = approx(x = .data$x,
                         y = .data$yhat,
                         xout = .data$x,
                         method = approx_method)$y) %>%
    ungroup() %>%
    filter(!.data$knot) %>%
    pull("yhat")

  return(data.frame(.pred = pred))
}
