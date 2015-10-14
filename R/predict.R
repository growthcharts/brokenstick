# predict.brokenstick.R

#' Predict growth curve according to the broken stick model
#'
#' For a given child, extract the conditional modes of the
#' random effects as specified by the broken stick (=linear
#' piecewise spline) model. 
#' @aliases predict.brokenstick
#' @param object   An object of either class \code{brokenstick} or
#' of \code{brokenstick.export}
#' containing the estimated parameters of the broken stick model
#' @param y     A vector with measurements 
#' @param age   A vector with decimal ages of length \code{length(y)}
#' @param \dots Additional arguments (not used)
#' @return A data frame with \code{length(slot(object, "knots"))
#' + slot(object, "degree")} elements with 
#' predicted values
#' @author Stef van Buuren, 2015
#' @export
predict.brokenstick <- function(object, y, age, ...) {
	
	# test for object type
	export <- NULL
	if (inherits(object, "brokenstick")) export <- export.brokenstick(object)
	if (inherits(object, "brokenstick.export")) export <- object
	if (is.null(export)) 
	  stop("Argument 'object' should be either of class 'brokenstick' or 'brokenstick.export'")
	
	if (missing(y)) return(NULL)
	if (missing(age)) return(NULL)
	
	# code the ages at which the child is observed as
	# linear splines with given break ages
	X <- bs(x = age, knots = object$knots, 
	        Boundary.knots = object$Boundary.knots, 
	        degree = object$degree)
	colnames(X) <- paste("x", 1:ncol(X), sep = "")

	# calculate random effect through empirical Bayes (BLUP) predictor
	bs.z <- EB(export, y = y, X, BS = TRUE)
	return(bs.z)
}
