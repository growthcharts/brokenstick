# scales.R

#' Convert the Z-scores scale of the broken stick model into original scale
#' 
#' @aliases convert2y
#' @param yname a string (e.g. \code{"hgt"} or \code{"wgt"}) indicating the outcome variable
#' @param z The broken stick values in the Z-score scale
#' @param sex either \code{"male"} or \code{"female"}
#' @param breaks a vector of internal knots
#' @param Boundary.knots vector of external knots
#' @return A \code{matrix} of the same dimensions as \code{bs}
#' @export
convert2y <- function(yname = "hgt", 
					  z,
					  sex, 
					  breaks = round(c(0, 28/365.25, 56/365.25, 
					  				 1/4, 1/3, 1/2, 7.5/12,
					  				 9/12, 11/12, 14/12, 18/12, 2), 4),
					  Boundary.knots = c(0, 3)) {
	z <- as.vector(z)
	x <- rep(c(breaks, Boundary.knots[2]), each = length(z))
	sex <- rep(sex, length(breaks) + 1)
	y <- z2y(z = z, x = x, ref = "nl1997", yname = yname,
			 sex = sex, sub = 'NL', drop = TRUE)
	y <- matrix(y, nrow = length(z))
	colnames(y) <- paste(yname, 0:length(breaks), sep = "")
	y
}
