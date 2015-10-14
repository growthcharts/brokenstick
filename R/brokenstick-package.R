# brokenstick-package.R
# 

#'@importFrom AGD y2z z2y
#'@importFrom lme4 lmer fixef ranef VarCorr lmerControl
#'@importFrom splines bs
#'@importFrom methods setGeneric setMethod
NULL

#' \pkg{brokenstick}: A package for irregular longitudinal data.
#' 
#' The broken stick model describes a set of individual curves 
#' by a linear mixed model using first order linear B-splines. The 
#' main use of the model is to align irregularly observed data to a 
#' user-specified grid of break ages. 
#' 
#' The \pkg{brokenstick} package contains functions for 
#' fitting a broken stick 
#' model to data, for exporting the parameters of the model for 
#' independent use outside this package, and for predicting broken 
#' stick curves for new data.
#' 
#' @section brokenstick functions:
#' The main functions are:
#' \tabular{ll}{
#'   \code{brokenstick()} \tab Fit a broken stick model to irregular data\cr
#'   \code{predictbs()} \tab Predict growth curve according to the broken stick model\cr
#'   \code{export.brokenstick()} \tab Export the estimates of a fitted brokenstick
#'    model object
#'   }
#' @docType package
#' @name brokenstick-pkg
#' @seealso \code{\link{brokenstick}}, 
#' \code{\link{EB}}, \code{\link{predictbs}}, 
#' \code{\link{export.brokenstick}}, \code{\link{predict.brokenstick}}
#' @references 
#' van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data}. Chapman & Hall/CRC, 2012. Chapter 9.
NULL

