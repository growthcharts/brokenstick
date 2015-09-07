# brokenstick.R
# 

#'@importFrom AGD y2z z2y
#'@importFrom lme4 lmer fixef ranef VarCorr lmerControl
#'@importFrom splines bs
NULL

#' \pkg{brokenstick}: A package for irregular longitudinal data.
#' 
#' The broken stick model describes a set of individual curves 
#' by a linear mixed model using first order linear B-splines. The 
#' main use of the model is to align irregularly observed data to a 
#' user-specified grid of break ages. A unique feature of this 
#' package is that all fitting is done in the Z-score scale, so 
#' fitting nonlinearities and irregular data can be treated as separate 
#' problems. The \pkg{brokenstick} package contains functions for 
#' fitting a broken stick 
#' model to data, for exporting the parameters of the model for 
#' independent use outside this package, and for predicting broken 
#' stick curves for new data.
#' 
#' @section brokenstick functions:
#' The main functions are:
#' \tabular{ll}{
#'   \code{make.basis()} \tab Create linear splines basis\cr
#'   \code{fit.brokenstick()} \tab Fit a broken stick model to irregular data\cr
#'   \code{EB()} \tab Random effect estimation by the Best Linear Unbiased Predictor (BLUP)\cr
#'   \code{predictbs()} \tab Predict growth curve according to the broken stick model\cr
#'   \code{export.brokenstick()} \tab Export the estimates of a fitted \code{lmer()} model object
#'   }
#' @docType package
#' @name brokenstick
#' @seealso \code{\link{make.basis}}, \code{\link{fit.brokenstick}}, 
#' \code{\link{EB}}, \code{\link{predictbs}}, 
#' \code{\link{export.brokenstick}}
#' @references 
#' van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data}. Chapman & Hall/CRC, 2012. Chapter 9.
NULL
