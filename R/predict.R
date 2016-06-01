# predict.R

#' Predict growth curve according to the broken stick model
#'
#' For a given child, extract the conditional modes of the
#' random effects as specified by the broken stick (=linear
#' piecewise spline) model. 
#' @aliases predict.brokenstick
#' @param object   An object of class \code{brokenstick}
#' @param y     A vector with measurements 
#' @param age   A vector with decimal ages of length \code{length(y)}
#' @param type  If \code{type = "curve"} (the default) 
#' the function returns the broken stick estimates. 
#' If \code{type = "response"}, the function returns a predicted value 
#' for each element of \code{y} determined by linear interpolation.
#' @param \dots Additional arguments (not used)
#' @return A data frame with \code{length(slot(object, "knots"))
#' + slot(object, "degree")} elements with 
#' predicted values
#' @author Stef van Buuren, 2015
#' @export
predict.brokenstick <- function(object, y, age, type = "curve", ...) {
	export <- export.brokenstick(object)
	predict(export, y, age, type = type, ...)
}


#' Predict growth curve according to the broken stick model
#'
#' For a given child, extract the conditional modes of the
#' random effects as specified by the broken stick (=linear
#' piecewise spline) model. 
#' @aliases predict.brokenstick.export
#' @param object   An object of class \code{brokenstick.export}
#' containing the estimated parameters of the broken stick model
#' @param y     A vector with measurements 
#' @param age   A vector with decimal ages of length \code{length(y)}
#' @param type  If \code{type = "curve"} (the default) 
#' the function returns the broken stick estimates. 
#' If \code{type = "response"}, the function returns a predicted value 
#' for each element of \code{y}.
#' @param \dots Additional arguments (not used)
#' @return A data frame with \code{length(slot(object, "knots"))
#' + slot(object, "degree")} elements with 
#' predicted values
#' @author Stef van Buuren, 2015
#' @export
predict.brokenstick.export <- function(object, y, age, type = "curve", ...) {
  
  type <- match.arg(type, c("curve", "response"))
  
  if (missing(y)) return(NULL)
  if (missing(age)) return(NULL)
  
  if (length(y) == 0 | length(age) == 0) return(numeric(0))
  
  # code the ages at which the child is observed as
  # linear splines with given break ages
  X <- bs(x = age, knots = object$knots, 
          Boundary.knots = object$Boundary.knots, 
          degree = object$degree)
  colnames(X) <- paste("x", 1:ncol(X), sep = "")
  
  # calculate random effect through empirical Bayes (BLUP) predictor
  bs.z <- EB(object, y = y, X, BS = TRUE)
  if (type == "curve") return(bs.z)
  
  # individual (response) prediction
  if (object$degree > 1) stop("Prediction uses linear interpolation for degree > 1")
  brk <- c(object$knots, object$Boundary.knots[2])
  approx(x = brk, y = bs.z, xout = age)$y
}


