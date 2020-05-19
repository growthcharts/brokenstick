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
#' @param knotnames Should the column names be the knots?
#' @param x_name Name of the variable, for labelling
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
                       warn = TRUE,
                       knotnames = FALSE,
                       x_name = "x") {

  # safety check: remove lower boundary knot from knots to be compatiable
  # with models fitted prior to version 0.53
  knots <- knots[knots > boundary[1L] & knots < boundary[2L]]

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
        knots = c(boundary[1L], knots),
        Boundary.knots = boundary,
        degree = degree
      )
    )
  }
  if (!knotnames) colnames(X) <- paste0("x", 1:ncol(X))
  else {
    colnames(X) <- as.character(sort(unique(c(boundary, knots))))
    colnames(X) <- paste(x_name, colnames(X), sep = "_")
  }

  # restore original if padded
  if (padx) X <- X[-1L, , drop = FALSE]

  return(X)
}
