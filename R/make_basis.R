#' Create linear splines basis
#'
#' This function creates the basis function of a linear first-order splines
#' at a user-specific set of break points. The default knots
#' code the age range 0-3 years.
#' @aliases make_basis
#' @param x a vector of ages at which the basis function are needed
#' @param knots a vector of internal knots, excluding boundary knots
#' @param boundary vector of external knots
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is \code{degree = 1}.
#' @param warn a logical indicating whether warnings from \code{splines::bs()}
#' should be given.
#' @return A matrix with \code{length(x)} rows and \code{length(breaks)}
#' columns, with some extra attributes described by \code{bs()}.
#' @author Stef van Buuren, 2017
#' @note Before version 0.54, it was standard practice that the \code{knots}
#' array always included \code{boundary[1]}.
#' @export
make_basis <- function(x,
                       knots = NULL,
                       boundary = range(x),
                       degree = 1,
                       warn = TRUE) {

  # safety check: remove lower boundary knot from knots to be compatiable
  # with models fitted prior to version 0.53
  knots <- knots[knots > boundary[1] & knots < boundary[2]]

  # trick to evade error from bs() if x is fully NA
  padx <- all(is.na(x))
  if (padx) x <- c(0, x)

  if (warn) {
    X <- splines::bs(
      x = x,
      knots = c(boundary[1], knots),
      Boundary.knots = boundary,
      degree = degree
    )
  } else {
    suppressWarnings(
      X <- splines::bs(
        x = x,
        knots = c(boundary[1], knots),
        Boundary.knots = boundary,
        degree = degree
      )
    )
  }
  colnames(X) <- paste0("x", 1:ncol(X))

  # restore original if padded
  if (padx) X <- X[-1, , drop = FALSE]

  return(X)
}
