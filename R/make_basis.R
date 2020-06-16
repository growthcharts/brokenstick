#' Create linear splines basis
#'
#' This function creates the basis function of a linear first-order splines
#' at a user-specific set of break points. The default knots
#' code the age range 0-3 years.
#' @aliases make_basis
#' @param x a \code{data.frame} with one column
#' @param knots a vector of internal knots, excluding boundary knots
#' @param boundary vector of external knots
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is \code{degree = 1}.
#' Setting \code{degree = 0} yields (crisp) dummy coding, and one
#' column less than for \code{degree = 1}.
#' @param warn a logical indicating whether warnings from \code{splines::bs()}
#' should be given.
#' @param knotnames Should the column names be the knots?
#' @return A matrix with \code{length(x)} rows and \code{length(breaks)}
#' columns, with some extra attributes described by \code{bs()}.
#' @author Stef van Buuren, 2020
#' @note Before version 0.54, it was standard practice that the \code{knots}
#' array always included \code{boundary[1L]}.
#' @examples
#' knots <- c(58, 64, 68, 72)
#' d1 <- make_basis(data.frame(hgt = women$height), knots = knots)
#' d0 <- make_basis(data.frame(hgt = women$height), knots = knots, degree = 0)
#' @export
make_basis <- function(x,
                       knots = NULL,
                       boundary = range(x),
                       degree = 1L,
                       warn = TRUE,
                       knotnames = TRUE) {

  pull.numeric <- function(.data, ...) as.vector(.data)

  # safety check: remove lower boundary knot from knots to be compatiable
  # with models fitted prior to version 0.53
  knots <- knots[knots > boundary[1L] & knots < boundary[2L]]

  # trick to evade error from bs() if x is fully NA
  padx <- all(is.na(x))
  if (padx) x <- c(0, x)

  x_name <- colnames(x)[1]

  # dummy coding if degree is zero
  if (degree == 0L) {
    df <- data.frame(x = cut(pull(x, x_name),
                             breaks = c(boundary[1L], knots, boundary[2L]),
                             right = FALSE, include.lowest = TRUE))
    X <- model.matrix(as.formula("~ 0 + x"), data = df)
  }

  # fuzzy coding by linear spline
  if (degree >= 1L) {
    if (warn) {
      X <- splines::bs(
        x = pull(x, x_name),
        knots = c(boundary[1L], knots),
        Boundary.knots = boundary,
        degree = degree
      )
    } else {
      suppressWarnings({
        X <- splines::bs(
          x = pull(x, x_name),
          knots = c(boundary[1L], knots),
          Boundary.knots = boundary,
          degree = degree
        )
      }
      )
    }
  }

  if (!knotnames) colnames(X) <- paste0("x", 1L:ncol(X))
  else {
    knots <- sort(unique(c(boundary, knots)))
    if (degree == 0L) knots <- knots[-length(knots)]
    colnames(X) <- as.character(knots)
    colnames(X) <- paste(x_name, colnames(X), sep = "_")
  }

  # restore original if padded
  if (padx) X <- X[-1L, , drop = FALSE]

  return(X)
}
