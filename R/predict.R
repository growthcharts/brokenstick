# predict.brokenstick.R

#' Predict growth curve according to the broken stick model
#'
#' For a given child, extract the conditional modes of the
#' random effects as specified by the broken stick (=linear
#' piecewise spline) model. 
#' @aliases predict.brokenstick
#' @param model   An object of either class \code{brokenstick} or
#' of \code{brokenstick.export}
#' containing the estimated parameters of the broken stick model
#' @param y     A vector with measurements 
#' @param age   A vector with decimal ages of length \code{length(y)}
#' @return A data frame with \code{length(slot(model, "knots"))
#' + slot(model, "degree")} elements with 
#' predicted values
#' @author Stef van Buuren, 2015
#' @export
predict.brokenstick <- function(model, y, age) {
	
	# test for object type
	export <- NULL
	if (inherits(model, "brokenstick")) export <- export.brokenstick(model)
	if (inherits(model, "brokenstick.export")) export <- model
	if (is.null(export)) 
	  stop("Argument 'model' should be either of class 'brokenstick' or 'brokenstick.export'")
	
	if (missing(y)) return(NULL)
	if (missing(age)) return(NULL)
	
	# code the ages at which the child is observed as
	# linear splines with given break ages
	X <- bs(x = age, knots = model$knots, 
	        Boundary.knots = model$Boundary.knots, 
	        degree = model$degree)
	colnames(X) <- paste("x", 1:ncol(X), sep = "")

	# calculate random effect through empirical Bayes (BLUP) predictor
	bs.z <- EB(export, y = y, X, BS = TRUE)
	return(bs.z)
	
	# convert back to y-scale
	age <- c(export$Boundary.knots[1], export$knots, export$Boundary.knots[2])
	if (package == "clopus") {
		# THIS PART IS SPECIFIC FOR groeivoorspeller
		# transform broken stick estimate back into original scale
		bs.y <- clopus::z2y(z = bs.z, x = age,
					ref = get("nl1997", pos = "package:clopus"),
					yname = yname,
					sex = sex, sub = "NL", drop = TRUE)
		if (!is.na(ga) & ga <= 36)
			bs.y <- clopus::z2y(z = bs.z, x = age,
						ref = get("preterm", pos = "package:clopus"),
						yname = yname,
						sex = sex, sub = max(floor(ga), 25), drop = TRUE)
		# END PART
	} else {
		# Use WHO references from pkg AGD to convert to Z-scores
		bs.y <- z2y(z = bs.z, x = age, sex = thesex, 
					ref = get(theref, pos = "package:AGD"))
	}
	return(bs.y)
}
