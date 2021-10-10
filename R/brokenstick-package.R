#' \pkg{brokenstick}: A package for irregular longitudinal data.
#'
#' The broken stick model describes a set of individual curves
#' by a linear mixed model using second-order linear B-splines. The
#' main use of the model is to align irregularly observed data to a
#' user-specified grid of break ages.
#'
#' The \pkg{brokenstick} package contains functions for
#' fitting a broken stick model to data, for predicting broken
#' stick curves for new data, and for plotting the results.
#'
#' @section brokenstick functions:
#' The main functions are:
#' \tabular{ll}{
#'   \code{brokenstick()} \tab Fit a broken stick model to irregular data\cr
#'   \code{predict()} \tab Obtain predictions on new data\cr
#'   \code{plot()} \tab Plot observed and fitted trajectories by group \cr
#' }
#'
#' The following functions are user-oriented helpers:
#' \tabular{ll}{
#'   \code{fitted()} \tab Calculate fitted values\cr
#'   \code{get_knots()} \tab Obtain the knots from a broken stick model\cr
#'   \code{get_omega()} \tab Extract variance-covariance of random effects\cr
#'   \code{get_r2()} \tab Obtain proportion of explained variance \cr
#'   \code{residuals()} \tab Extract residuals from broken stick model\cr
#' }
#'
#' The following functions perform calculations:
#' \tabular{ll}{
#'    \code{set_control()}\tab Set controls to steer calculations\cr
#'    \code{control_kr()}\tab Set controls for the \code{kr} method\cr
#'    \code{kr()} \tab Kasim-Raudenbush sampler for two-level normal model \cr
#'    \code{make_basis()} \tab Create linear splines basis\cr
#' }
#'
#' @docType package
#' @name brokenstick-pkg
#' @seealso \code{\link{brokenstick}},
#' \code{\link{EB}}, \code{\link{predict.brokenstick}}
#' @note
#' Development of this package was kindly supported under the Healthy
#' Birth, Growth and Development knowledge integration (HBGDki)
#' program of the Bill & Melinda Gates Foundation.
#' @references
#' van Buuren, S. (2018). \emph{Flexible Imputation of Missing Data. Second Edition}. Chapman & Hall/CRC. Chapter 11.
#' \url{https://stefvanbuuren.name/fimd/sec-rastering.html#sec:brokenstick}
NULL
