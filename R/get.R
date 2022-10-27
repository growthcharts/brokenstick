#' Obtain the knots from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param whatknots Deprecated. Use `hide` instead.
#' @param what Deprecated. Use `hide` instead.
#' @inheritParams brokenstick
#' @return A vector with knot locations, either both, internal only or
#' boundary only, depending on `hide`.
#' The result is \code{NULL} if \code{object} does not
#' have proper class. Returns \code{numeric(0)} if
#' there are no internal knots.
#' @examples
#' get_knots(fit_200, hide = "bo")
#' @export
get_knots <- function(object,
                      hide = c("right", "left", "boundary", "internal", "none"),
                      whatknots = "all",
                      what = "all") {

  stopifnot(inherits(object, c("brokenstick")))

  if (!missing(what) || !missing(whatknots)) {
    warning("arguments 'what' and 'whatknots' in 'get_knots()' are deprecated; please use 'hide' instead.",
            call. = FALSE)
    whatknots <- what
    object$hide <- switch(whatknots,
                          all = "none",
                          internal = "boundary",
                          boundary = "internal",
                          dropfirst = "left",
                          droplast = "right",
                          "right")
  }

  if (!missing(hide)) {
    hide <- match.arg(hide)
  } else {
    hide <- ifelse(is.null(object$hide), "right", object$hide)
  }

  internal <- object$internal
  # legacy for objects created before v2.0
  if (is.null(internal)) internal <- object$knots
  boundary <- object$boundary
  internal <- internal[internal > boundary[1L] & internal < boundary[2L]]

  result <- switch(hide,
                   none = c(boundary[1L], internal, boundary[2L]),
                   internal = boundary,
                   boundary = internal,
                   left = c(internal, boundary[2L]),
                   right = c(boundary[1L], internal))
  return(result)
}

#' Extract Variance and Correlation Components
#'
#' Extracts variance-covariance or correlation matrix from a
#' `brokenstick` object.
#'
#' @param x      Object of class `brokenstick`
#' @param cor    Logical. Should the function return the correlation matrix
#' instead of the covariance matrix? The default is `FALSE`.
#' @param what  Deprecated.
#' @param whatknots Deprecated.
#' @inheritParams brokenstick
#' @return A numeric matrix, possibly with zero rows and columns if no names match
#' @examples
#' f1 <- brokenstick(hgt_z ~ age | id, smocc_200[1:1000, ], knots = 0:2, seed = 1)
#' get_omega(f1, cor = TRUE, hide = "boundary")
#' @export
get_omega <- function(x,
                      hide = c("right", "left", "boundary", "internal", "none"),
                      cor = FALSE,
                      whatknots = "all",
                      what = "cov") {
  stopifnot(inherits(x, "brokenstick"))

  if (!missing(what)) {
    warning("argument 'what' in 'get_omega()' is deprecated; please use 'cor' instead.",
            call. = FALSE)
    if (missing(cor)) {
      cor <- ifelse(what == "cor", TRUE, FALSE)
    }
  }

  if (!missing(whatknots)) {
    warning("argument 'whatknots' in 'get_omega()' is deprecated; please use 'hide' instead.",
            call. = FALSE)
    whatknots <- what
    x$hide <- switch(whatknots,
                     all = "none",
                     internal = "boundary",
                     boundary = "internal",
                     dropfirst = "left",
                     droplast = "right",
                     "right")
  }

  if (!missing(hide)) {
    hide <- match.arg(hide)
  } else {
    hide <- ifelse(is.null(x$hide), "right", x$hide)
  }

  omega <- x$omega
  v <- colnames(omega)
  nameset <- switch(hide,
                    none = v,
                    boundary = v[c(-1L, -length(v))],
                    internal = v[c(1L, length(v))],
                    left = v[-1L],
                    right = v[-length(v)])
  if (length(nameset)) {
    omega <- omega[nameset, nameset, drop = FALSE]
  } else {
    omega <- matrix(NA_real_, 0L, 0L)
  }

  # return correlation matrix if requested
  if (cor) {
    if (dim(omega)[1L]) {
      return(cov2cor(omega))
    }
  }

  # covariance matrix
  return(omega)
}


#' Obtain proportion of explained variance from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param newdata Data on which `r.squared` must be calculated
#' @return Proportion of explained variance
#' @examples
#' get_r2(fit_200)
#' get_r2(fit_200_light, newdata = smocc_200)
#' @export
get_r2 <- function(object, newdata = NULL) {
  newdata <- get_newdata(object, newdata)
  p <- predict(object, newdata = newdata)
  nd <- newdata %>%
    select(object$names$y) %>%
    bind_cols(p) %>%
    tidyr::drop_na()
  return(cor(nd[[".pred"]], nd[[object$names$y]])^2)
}


get_newdata <- function(x, newdata) {
  # sets the newdata argument
  stopifnot(inherits(x, "brokenstick"))
  if (is.null(newdata) && x$light) {
    stop("Argument 'newdata' is required for a light brokenstick object.", call. = FALSE)
  }
  if (is.null(newdata) && !x$light) {
    newdata <- x$data
  }
  stopifnot(is.data.frame(newdata) || is.matrix(newdata))
  return(newdata)
}
