# predictbs.R

#' Predict growth curve according to the broken stick model
#'
#' For a given child, extract the conditional modes of the
#' random effects as specified by the broken stick (=linear
#' piecewise spline) model. Modeling is done in the scale of the
#' Z-score. The code assumes that the estimated parameters of the
#' relevant broken stick model (fixed effect, variance-covariance of
#' random effect, and residual variance) are available in a special object
#' of class \code{brokenstick.export}.
#' @aliases predictbs
#' @param model   An object of either class \code{lmerMod} or
#' class \code{brokenstick.export}
#' containing the estimated parameters of the broken stick model
#' for the type of measurement in \code{yname}
#' @param yname A string (\code{"hgt"}, \code{"wgt"} or \code{"hdc"})
#' identifying the type of measurement
#' @param y     A vector with measurements belonging to one person
#' @param age   A vector with decimal ages of length \code{length(y)}
#' @param sex   Either \code{"male"} or \code{"female"}
#' @param ga    Numeric, gestational age in weeks.
#' The default is 40 weeks, term birth.
#' @param zscale A logical indicating whether the result should be in
#' the Z-scale (\code{zscale = TRUE}) or Y-scale (\code{zscale = FALSE}).
#' The default is \code{FALSE}.
#' @return A \code{vector} with a length equal to the number of break
#' points (i.e. number of unique knots)
#' @author Stef van Buuren, 2015
#' @export
predictbs <- function(model, yname, y, age, sex, ga = 40,
						  zscale = FALSE){
	
	# requires medMod object fitted by lmer()
	export <- NULL
	if (inherits(model, "lmerMod")) export <- export.brokenstick(model)
	if (inherits(model, "brokenstick.export")) export <- model
	if (is.null(export)) stop("Argument 'model' should be either of class 'lmerMod' or 'brokenstick.export'")
	
	# turn y into Z-scores using the Dutch 1997 references
	# or preterm reference for those with ga <= 36
	z <- y2z(y = y, x = age, ref = "nl1997", yname = yname,
			 sex = sex, sub = "NL", drop = TRUE)
	if (!is.na(ga) & ga <= 36)
		z <- y2z(y = y, x = age, ref = "preterm", yname = yname,
				 sex = sex, sub = max(floor(ga), 25), drop = TRUE)
	
	# code the ages at which the child is observed as
	# linear splines with given break ages
	X <- make.basis(age,
					 knots = model$knots,
					 Boundary.knots = model$Boundary.knots)
	
	# calculate random effect through empirical Bayes (BLUP) predictor
	bs.z <- EB(model, y = z, X, BS = TRUE)
	if (zscale) return(bs.z)
	
	# transform broken stick estimate back into original scale
	bs.y <- z2y(z = bs.z, x = age, ref = "nl1997", yname = yname,
				sex = sex, sub = "NL", drop = TRUE)
	if (!is.na(ga) & ga <= 36)
		bs.y <- z2y(z = bs.z, x = age, ref = "preterm", yname = yname,
					sex = sex, sub = max(floor(ga), 25), drop = TRUE)
	
	return(bs.y)
}
