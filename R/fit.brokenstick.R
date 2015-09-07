#' Fit a broken stick model to irregular data
#' 
#' The broken stick model models an irregularly observed series 
#' of measurement by scaling them onto a user-specified set of 
#' 'ideal' ages. The model codes age by a series of linear B-splines.
#' Differences between persons are expressed by a random effect 
#' pertaining to each knot. On the individual level, each 
#' modeled growth curve connect straight lines that join at the 
#' chosen break ages, and hence look like a 'broken stick'.
#' 
#' @details 
#' Relations over time are modeled by the variance-covariance 
#' parameters of the random effects. Currently, this matrix is estimated
#' as unstructured by \code{lme()}. Experience has shown that if 
#' there are enough children relative to the number of specified 
#' random effects, the variance-variance matrix will have elements
#' that diminish as they move away from the diagonal, as one would
#' expect for data without seasonality, like growth data.
#' 
#' This function can be time consuming for data sets with several hundreds
#' of children.
#' @aliases fit.brokenstick
#' @param z a vector containing the measurements in the Z-scale
#' @param age a vector of \code{length(z)} awith decimal age
#' @param id a vector 
#' @param control A function to control fitting of \code{lmer}. The default
#' is set to \code{lmerControl(check.nobs.vs.nRE = "warning")}, which turn
#' fatal errors with respect the number of parameters into warnings.
#' @param \dots Additional arguments passed down to \code{make.basis()} 
#' (e.g. to specify other knots) and \code{lmer()} (e.g. to specify additional 
#' \code{lmer()} options.
#'  @return The fitted model of class \code{lmerMod}
#' @examples 
#' library(mice)
#' data <- tbc[tbc$id < 1000 & tbc$age < 2.5,]
#' fit <- fit.brokenstick(z = data$hgt.z, age = data$age, id = data$id)
#' plot(fit)
#' @export
fit.brokenstick <- function(z, age, id, 
							control = lmerControl(check.nobs.vs.nRE = "warning"), 
							...) {
    X <- make.basis(x = age, ...)
    nknots <- ncol(X)
    data <- na.omit(data.frame(id = id, age = age, z = z, X))
    f <- as.formula(paste("z", "~", 
                          "0 + ", paste0("x", 0:(nknots-1), collapse = " + "),
                          "+ (", 
                          "0 + ", paste0("x", 0:(nknots-1), collapse = " + "),
                          "| id)"))
    fit <- lmer(f, data = data,
                control = lmerControl(check.nobs.vs.nRE = "warning"),
    			...)
    attr(fit, "knots") <- attr(X, "knots")
    attr(fit, "Boundary.knots") <- attr(X, "Boundary.knots")
    attr(fit, "model") <- "brokenstick"
    return(fit)
}
