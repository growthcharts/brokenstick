#' Export the estimates of a fitted lmer() model
#' 
#' Exports the crucial estimates of a fitted lmer() model so that the 
#' stored estimates can be used by the EB() function 
#' to calculate random effect estimates for new user data.
#' 
#' @aliases export.brokenstick
#' @param model An object of class \code{lmerMod} or class 
#' \code{brokenstick.export} (typically generated 
#' by a previous call to \code{export.brokenstick()}).
#' @return A \code{list} of class \code{brokenstick.export}, with elements corresponding to the estimates parameters of the fitted model.
#' @export
export.brokenstick <- function(model) {
	
	# if already a broken.stick.export object, do nothing
	if (inherits(model, "brokenstick.export")) return(model)
	
	if (!inherits(model, "lmerMod")) 
		stop("Argument 'model' expected as class 'lmerMod' or 'brokenstick.export'")
	
	# extract estimates from merMod object
	beta <- fixef(model)
	omega <- as.matrix(as.data.frame(VarCorr(model)$id))  # variance of RE, Q*Q
	df <- as.data.frame(VarCorr(model))
	sigma2 <- df[df$grp == "Residual", "vcov"]
	
	z <- list(beta = beta, omega = omega, sigma2 = sigma2,
			  knots = attr(model, "knots"), 
			  Boundary.knots = attr(model, "Boundary.knots"))
	class(z) <- "brokenstick.export"
	return(z)
}

