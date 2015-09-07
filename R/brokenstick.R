# brokenstick.R
# 

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

#' Calculated broken stick values from a fitted broken stick model
#' 
#' The broken stick values are simply the sum of the fixed and random 
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

#' Empirical Bayes predictor for random effects
#' 
#' This function can estimate random effect for a given set of 
#' model estimates and new user data. Contrary to many current 
#' implementations for random effect prediction, the unit may be 
#' new to the model. The methods implements the 
#' EB estimate (also known as BLUP) 
#' as described in Skrondral and Rabe-Hasketh, 2009, p. 683. 
#' This function can also provide the broken stick estimate for a given level, 
#' which is simply the sum of (global) fixed and (level) random effects
#' The current implementation does not provide prediction errors.
#' 
#' @aliases EB
#' @param model An object of class \code{lmerMod} (typically created by
#' \code{fit.brokenstick()}) or class \code{brokenstick.export} 
#' (typically generated by \code{export.brokenstick()}).
#' @param y A vector of new measurements for unit j, scaled in the same metric as the fitted model.
#' @param X A \code{nj * p} matrix with fixed effects for unit j, typically produced by \code{make.basis()}.
#' @param Z A \code{nj * q} matrix with random effects for unit j. The default sets \code{Z} equal to \code{X}.
#' @param BS A logical indicating whether broken stick estimates should be
#' returned (\code{BS = TRUE}) or the random effects (\code{BS = FALSE}). 
#' The default is \code{FALSE}.
#' @return A vector of length q containing the random effect or broken stick  estimates for unit j.
#' @author Stef van Buuren, 2015
#' @references 
#' Skrondal, A., Rabe-Hesketh, S. (2009). 
#' Prediction in multilevel generalized linear models. 
#' J. R. Statist. Soc. A, 172, 3, 659-687.
#' @examples 
#' library(mice)
#' data <- tbc[tbc$id < 1000 & tbc$age < 2.5,]
#' fit <- fit.brokenstick(z = data$hgt.z, age = data$age, id = data$id)
#' #
#' # conventional random effect for child id 8
#' lme4::ranef(fit)$id[1,]
#' #
#' # EB estimate random effect for child id 8
#' est <- export.brokenstick(fit)
#' y <- data[data$id == 8, "hgt.z"]
#' y
#' X <- make.basis(data[data$id == 8, "age"])
#' X
#' EB(est, y, X)
#' @export
EB <- function (model, y, X, Z = X, BS = FALSE) {
    
    # make sure we get the exported model
    export <- export.brokenstick(model)

    # eliminate missing outcomes 
    select <- !is.na(y)
    
    # if there are no valid values left, return the fixed effect
    # as broken stick estimates
    if (!any(select)) return(export$beta)

    # get into shape for matrix multiplication
    y <- matrix(y[select], ncol = 1) # nj * 1
    Z <- as.matrix(Z[select,])  # nj * q
    X <- as.matrix(X[select,])  # nj * p
    beta <- matrix(export$beta, ncol = 1)
    
    # construct appropriate matrices
    sigma.inv <- solve(Z %*% export$omega %*% t(Z) + 
                           diag(export$sigma2, nrow(Z)))
    
    # calculate random effect by EB estimate
    re <- export$omega %*% t(Z) %*% sigma.inv %*% (y - X %*% beta)
    
    # calculate broken stick estimate by summing fixed and random parts
    if (BS) re <- export$beta + re
    
    return(as.vector(re))
}
