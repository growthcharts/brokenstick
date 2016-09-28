
#' Obtain the knots from a broken stick model
#'
#' @param object An object of class \code{brokenstick} or \code{brokenstick.export}
#' @return A vector with knot locations
#' @examples
#' get_knots(fit_hgt)
#' @export
get_knots <- function(object) {
  if (inherits(object, "brokenstick")) {
    knots <- c(object@knots, object@boundary[2])
    return(knots)
  }
  if (inherits(object, "brokenstick_export")) { 
    knots <- c(object$knots, object$boundary[2])
    return(knots)
  }
  return(NULL)
}

#' Obtain the x and y data from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param ids A vector specifying the id's of the persons. If omitted, all id's are included.
#' @return A data frame with subjid, x and y. The result is \code{NULL} if \code{object} is not of class \code{brokenstick}.
#' @examples
#' get_xy(fit_hgt, ids = c(10001, 10002))
#' @export
get_xy <- function(object, ids = NULL) {
  brk <- get_knots(object)
  if (inherits(object, "brokenstick")) {
    subjid <- model.frame(object)$subjid
    idx <- rep(TRUE, length(subjid))
    if (!is.null(ids)) idx <- subjid %in% ids
    data <- data.frame(subjid = subjid[idx],
                       x = model.matrix(object)[idx, , drop = FALSE] %*% brk,
                       y = model.frame(object)$y[idx])
    row.names(data) <- as.character(1:nrow(data))
    return(data)
  }
  return(NULL)
}

#' Obtain the X model matrix from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param ids A vector specifying the id's of the persons. If omitted, all id's are included.
#' @return A matrix with number of columns equal to the number of knots. 
#' The result is \code{NULL} if \code{object} is not of class \code{brokenstick}.
#' @examples
#' get_X(fit_hgt, ids = c(10001, 10002))
#' @export
get_X <- function(object, ids = NULL) {
  if (inherits(object, "brokenstick")) {
    if (is.null(ids)) return(model.matrix(object))
    subjid <- model.frame(object)$subjid
    idx <- subjid %in% ids
    return(model.matrix(object)[idx, , drop = FALSE])
  }
  return(NULL)
}

#' Obtain the y variable from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param ids A vector specifying the id's of the persons. If omitted, all id's are included.
#' @return A vector
#' The result is \code{NULL} if \code{object} is not of class \code{brokenstick}.
#' @examples
#' get_y(fit_hgt, ids = c(10001, 10002))
#' @export
get_y <- function(object, ids = NULL) {
  if (inherits(object, "brokenstick")) {
    if (is.null(ids)) return(model.frame(object)$y)
    subjid <- model.frame(object)$subjid
    idx <- subjid %in% ids
    return(model.frame(object)$y[idx])
  }
  return(NULL)
}
