#' Calculated broken stick estimates from a fitted model
#' 
#' The broken stick estimates are simply the sum of the fixed and random 
#' effect. This function return that sum for all levels.
#' @aliases get.brokenstick.values
#' @param fit The fitted model of class \code{lmerMod}, presumably fitted
#'  by \code{fit.brokenstick()}
#' @return Matrix with broken stick values
#' @export
get.brokenstick.values <- function(fit) {
	if (!inherits(fit, "lmerMod")) stop("Argument 'fit' not of class lmerMod")
	return(t(t(lme4::ranef(fit)$id) + lme4::fixef(fit)))
}

