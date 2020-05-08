#' Obtain the knots from a broken stick model
#'
#' @param object An object of class \code{brokenstick} or
#' \code{brokenstick.export}
#' @param what A character vector of length 1. Valid values are
#' \code{"all"}, \code{"knots"}, \code{"boundary"} or \code{"droplast"}.
#' The default is \code{what = "all"}.
#' @return A vector with knot locations, either both, internal only or
#' boundary only. The result is \code{NULL} if \code{object} does not
#' have proper class. The function can return \code{numeric(0)} if
#' there are no internal knots.
#' @examples
#' get_knots(fit_206, "knots")
#' @export
get_knots <- function(object,
                      what = c("all", "knots", "boundary", "droplast")) {

  if (!inherits(object, c("brokenstick", "brokenstick_export"))) return(NULL)

  what <- match.arg(what)
  if (inherits(object, "brokenstick")) {
    knots <- object@knots
    boundary <- object@boundary
  }
  if (inherits(object, "brokenstick_export")) {
    knots <- object$knots
    boundary <- object$boundary
  }

  internal <- knots[knots > boundary[1] & knots < boundary[2]]

  result <- switch(what,
                   all = c(boundary[1], internal, boundary[2]),
                   knots = internal,
                   boundary = boundary,
                   droplast = c(boundary[1], internal))
  return(result)
}

#' Obtain the x and y data from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param ids A vector specifying the id's of the persons. If omitted,
#' all id's are included.
#' @return A data frame with subjid, x and y. The result is \code{NULL}
#' if \code{object} is not of class \code{brokenstick}.
#' @examples
#' get_xy(fit_206, ids = c(10001, 10002))
#' @export
get_xy <- function(object, ids = NULL) {
  if (!inherits(object, "brokenstick")) return(NULL)
  if (is.null(ids)) return(object@xy)
  return(object@xy[object@xy$subjid %in% ids, ])
}

#' Obtain the X model matrix from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param ids A vector specifying the id's of the persons. If omitted,
#' all id's are included.
#' @return A matrix with number of columns equal to the number of
#' knots. The result is \code{NULL} if \code{object} is not of class
#' \code{brokenstick}.
#' @examples
#' get_X(fit_206, ids = c(10001, 10002))
#' @export
get_X <- function(object, ids = NULL) {
  if (!inherits(object, "brokenstick")) return(NULL)
  if (is.null(ids)) return(model.matrix(object))
  subjid <- model.frame(object)$subjid
  idx <- subjid %in% ids
  return(model.matrix(object)[idx, , drop = FALSE])
}

#' Obtain proportion of explained variance from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @return Proportion of explained variance
#' @examples
#' get_pev(fit_206)
#' @export
get_pev <- function(object) {
  if (!inherits(object, "brokenstick")) return(NULL)
  p <- predict(object)
  return(var(p$yhat, na.rm = TRUE) / var(p$y, na.rm = TRUE))
}
