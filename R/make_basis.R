#' Create linear splines basis
#'
#' This function creates the basis function of a second-order (linear) splines
#' at a user-specific set of break points.
#' @aliases make_basis
#' @param x numeric vector
#' @param xname predictor name. Default is \code{"x"}
#' @param internal a vector of internal knots, excluding boundary knots
#' @param boundary vector of external knots
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is \code{degree = 1}.
#' Setting \code{degree = 0} yields (crisp) dummy coding, and one
#' column less than for \code{degree = 1}.
#' @param warn a logical indicating whether warnings from \code{splines::bs()}
#' should be given.
#' @return A matrix with \code{length(x)} rows and \code{length(breaks)}
#' columns, with some extra attributes described by \code{bs()}.
#' @author Stef van Buuren, 2020
#' @note Before version 0.54, it was standard practice that the \code{knots}
#' array always included \code{boundary[1L]}.
#' @examples
#' knots <- c(58, 64, 68, 72)
#' d1 <- make_basis(women$height, xname = "hgt", internal = knots)
#' d0 <- make_basis(women$height, xname = "hgt", internal = knots, degree = 0)
#' @export
make_basis <- function(x,
                       xname = "x",
                       internal = NULL,
                       boundary = range(x),
                       degree = 1L,
                       warn = TRUE) {

  # safety check: remove lower boundary knot from knots to be compatiable
  # with models fitted prior to version 0.53
  internal <- internal[internal > boundary[1L] & internal < boundary[2L]]

  # trick to evade error from bs() if x is fully NA
  padx <- all(is.na(x))
  if (padx) x <- c(0, x)

  # dummy coding if degree is zero
  if (degree == 0L) {
    df <- data.frame(x = cut(x,
                             breaks = c(boundary[1L], internal, boundary[2L]),
                             right = FALSE, include.lowest = TRUE))
    X <- model.matrix(as.formula("~ 0 + x"),
                      model.frame(~ ., df, na.action = na.pass))
  }

  # fuzzy coding by linear spline
  if (degree >= 1L) {
    if (warn) {
      X <- splines::bs(
        x = x,
        knots = c(boundary[1L], internal),
        Boundary.knots = boundary,
        degree = degree
      )
    } else {
      suppressWarnings({
        X <- splines::bs(
          x = x,
          knots = c(boundary[1L], internal),
          Boundary.knots = boundary,
          degree = degree
        )
      }
      )
    }
  }

  knots <- sort(unique(c(boundary, internal)))
  if (degree == 0L) knots <- knots[-length(knots)]
  colnames(X) <- paste(xname, as.character(knots), sep = "_")

  # restore original if padded
  if (padx) X <- X[-1L, , drop = FALSE]

  return(X)
}
