# brokenstick-package.R
# 

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
#'   \code{conditional.means()} \tab Obtain the broken stick estimates\cr
#'   \code{EB()} \tab Empirical Bayes predictor for random effects\cr
#'   \code{predict()} \tab Predict new growth curve\cr
#'   \code{export.brokenstick()} \tab Export estimates of the model}
#' @docType package
#' @name brokenstick-pkg
#' @seealso \code{\link{brokenstick}}, 
#' \code{\link{EB}}, 
#' \code{\link{export.brokenstick}}, \code{\link{predict.brokenstick}}
#' @note 
#' Development of this package was kindly supported under the Healthy
#' Birth, Growth and Development knowledge integration (HBGDki)
#' program of the Bill & Melinda Gates Foundation.
#' @references 
#' van Buuren, S. (2012). \emph{Flexible Imputation of Missing Data}. Chapman & Hall/CRC, 2012. Chapter 9.
NULL

