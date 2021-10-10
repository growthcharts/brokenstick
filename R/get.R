#' Obtain the knots from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param what A character vector of length 1. Valid values are
#' @param \dots Not used
#' \code{"all"}, \code{"knots"}, \code{"boundary"} or \code{"droplast"}.
#' The default is \code{what = "all"}.
#' @return A vector with knot locations, either both, internal only or
#' boundary only. The result is \code{NULL} if \code{object} does not
#' have proper class. The function can return \code{numeric(0)} if
#' there are no internal knots.
#' @examples
#' get_knots(fit_200, "knots")
#' @export
get_knots <- function(object,
                      what = c("all", "knots", "boundary", "droplast"),
                      ...) {
  if (!inherits(object, c("brokenstick"))) {
    return(NULL)
  }

  what <- match.arg(what)
  if (inherits(object, "brokenstick")) {
    knots <- object$knots
    boundary <- object$boundary
  }

  internal <- knots[knots > boundary[1] & knots < boundary[2]]

  result <- switch(what,
    all = c(boundary[1], internal, boundary[2]),
    knots = internal,
    boundary = boundary,
    droplast = c(boundary[1], internal)
  )
  return(result)
}


#' Obtain proportion of explained variance from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param newdata Data on which `r.squared` must be calculated
#' @return Proportion of explained variance
#' @examples
#' get_r2(fit_200, smocc_200)
#' @export
get_r2 <- function(object, newdata) {
  if (!inherits(object, "brokenstick")) {
    stop("object not of class brokenstick")
  }

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
#' @return A numeric matrix, possibly with zero rows and columns if no names match
#' @examples
#' f1 <- brokenstick(hgt_z ~ age | id, smocc_200[1:1000, ], knots = 0:3, seed = 1)
#' get_omega(f1, "cor", c("age_1", "age_2"))
#' @export
get_omega <- function(x, what = c("cov", "cor"), names = NULL) {
  stopifnot(inherits(x, "brokenstick"))
  what <- match.arg(what)
  omega <- x$omega
  if (length(names) == 1L && names == "all") {
    names <- colnames(omega)
  }
  if (is.null(names)) {
    names <- colnames(omega)[1L:length(colnames(omega)) - 1L]
  }
  names <- intersect(names, colnames(omega))
  if (length(names)) {
    omega <- omega[names, names, drop = FALSE]
  } else {
    omega <- matrix(NA_real_, 0L, 0L)
  }
  if (what == "cov") return(omega)
  if (dim(omega)[1L]) return(cov2cor(omega))
  return(omega)
}
