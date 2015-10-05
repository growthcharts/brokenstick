

#' Calculated broken stick estimates from a fitted model
#' 
#' The broken stick estimates are the conditional means of the 
#' posterior distribution of the random effect. The means are 
#' calculated as the sum of the fixed and random 
#' effects. This function returns this sum at every subject 
#' at every breakpoint.
#' @aliases conditional.means
#' @param fit The fitted model of class \code{brokenstick}, 
#' as produced by \code{brokenstick()}
#' @return Matrix with conditional means. Each row corresponds to 
#' a subject, each column corresponds to an (internal) knot.
#' @export
conditional.means <- function(fit) {
	if (!inherits(fit, "lmerMod")) stop("Argument 'fit' not of class 'lmerMod' or of class 'brokenstick'")
	return(t(t(lme4::ranef(fit)$subject) + lme4::fixef(fit)))
}

