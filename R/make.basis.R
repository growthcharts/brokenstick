#' Create linear splines basis 
#' 
#' This function creates the basis function of a linear first-order splines
#' at a user-specific set of break points. The default knots 
#' code the age range 0-3 years.
#' @aliases make.basis
#' @param x a vector of ages at which the basis function are needed
#' @param knots a vector of internal knots
#' @param Boundary.knots vector of external knots
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is \code{degree = 1}.
#' @return A matrix with \code{length(x)} rows and \code{length(breaks)}
#' columns, with some extra attributes described by \code{bs()}.
#' @author Stef van Buuren, 2015
#' @export
make.basis <- function(x, 
                       knots = round(c(0, 28/365.25, 56/365.25, 
                                        1/4, 1/3, 1/2, 7.5/12,
                                        9/12, 11/12, 14/12, 18/12, 2), 4),
                       Boundary.knots = c(0, 3),
					   degree = 1) {
    # calculate break points 0-2 years as birth + standard visits 0-12 yr
    X <- bs(x, knots = knots, 
            Boundary.knots = Boundary.knots, degree = degree)
    dimnames(X)[[2]] <- paste("x", 0:(ncol(X)-1), sep = "")
    X
}

