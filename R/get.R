#' Obtain the knots from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param whatknots A character vector of length 1 specifies the knot set.
#' Valid values are \code{"all"}, \code{"internal"}, \code{"boundary"},
#' \code{"dropfirst"} and \code{"droplast"}. The default is
#' \code{whatknots = "all"}
#' @param what Deprecated. Use `whatknots` instead.
#' @return A vector with knot locations, either both, internal only or
#' boundary only. The result is \code{NULL} if \code{object} does not
#' have proper class. Returns \code{numeric(0)} if
#' there are no internal knots.
#' @examples
#' get_knots(fit_200, "internal")
#' @export
get_knots <- function(object,
                      whatknots = c("all", "internal", "boundary", "dropfirst", "droplast"),
                      what = "all") {
  stopifnot(inherits(object, c("brokenstick")))
  if (!missing(what)) {
    warning("argument what is deprecated; please use whatknots instead.",
            call. = FALSE)
    whatknots <- what
  }
  whatknots <- match.arg(whatknots)
  internal <- object$internal
  # legacy for objects created before v2.0
  if (is.null(internal)) internal <- object$knots
  boundary <- object$boundary
  internal <- internal[internal > boundary[1L] & internal < boundary[2L]]

  result <- switch(whatknots,
                   all = c(boundary[1L], internal, boundary[2L]),
                   internal = internal,
                   boundary = boundary,
                   dropfirst = c(internal, boundary[2L]),
                   droplast = c(boundary[1L], internal)
  )
  return(result)
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

#' Extract Variance and Correlation Components
#'
#' Extracts variance-covariance or correlation matrix from a
#' `brokenstick` object.
#'
#' @param x      Object of class `brokenstick`
#' @param what   Either `"cov"` (default) for the covariance matrix,  or `"cor"`
#'  for the correlation matrix.
#' @param names  A vector of column names of. If not specified, the function
#'  automatically drops the entries corresponding to the right boundary. Specify
#'  `names = "all"` to prevent dropping.
#' @inheritParams get_knots
#' @return A numeric matrix, possibly with zero rows and columns if no names match
#' @examples
#' f1 <- brokenstick(hgt_z ~ age | id, smocc_200[1:1000, ], knots = 0:2, seed = 1)
#' get_omega(f1, what = "cor", names = c("age_1", "age_2"))
#' @export
get_omega <- function(x,
                      what = c("cov", "cor"),
                      whatknots = c("all", "internal", "boundary", "dropfirst", "droplast"),
                      names = NULL) {
  stopifnot(inherits(x, "brokenstick"))
  what <- match.arg(what)
  whatknots <- match.arg(whatknots)

  omega <- x$omega
  v <- colnames(omega)
  nameset <- switch(whatknots,
                    all = v,
                    internal = v[c(-1L, -length(v))],
                    boundary = v[c(1L, length(v))],
                    dropfirst = v[-1L],
                    droplast = v[-length(v)])
  if (!is.null(names)) {
    nameset <- intersect(names, v)
  }
  if (length(names) == 1L && names == "all") {
    nameset <- v
  }
  if (length(nameset)) {
    omega <- omega[nameset, nameset, drop = FALSE]
  } else {
    omega <- matrix(NA_real_, 0L, 0L)
  }
  if (what == "cov") {
    return(omega)
  }
  if (dim(omega)[1L]) {
    return(cov2cor(omega))
  }
  return(omega)
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
