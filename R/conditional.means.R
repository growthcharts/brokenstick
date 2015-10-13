

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
#' @param long A logical indicating whether the result should be in the 
#' long form (the default), or in the broad form.
#' @return If \code{long} is \code{TRUE}, a matrix with three columns, representing the 
#'  subject level, breakpoint value and the conditional mean of the 
#'  distribution of broken stick values. 
#' If \code{long} is \code{FALSE}, a matrix with 
#' \code{length(knots) + degree} columns. Each row corresponds to 
#' a subject, each column corresponds to a knot.
#' @export
conditional.means <- function(fit, long = TRUE) {
	if (!inherits(fit, "brokenstick")) 
	  stop("Argument 'fit' not of class 'brokenstick'")
  z <- t(lme4::ranef(fit)$subject) + lme4::fixef(fit)
  
  # return broad form (n, (length(knots) + degree))
  if (!long) return(t(z))
  
  # return long form (n * (length(knots) + degree), 4)
  brk <- c(fit@knots, fit@Boundary.knots[2])
  grd <- expand.grid(age = brk, id = as.factor(rownames(lme4::ranef(fit)$subject)))
  data.frame(grd, hgt.z = as.vector(z))
}

