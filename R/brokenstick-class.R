#' Class `brokenstick`
#'
#' The main fitting function [brokenstick()] returns an object of class
#' `brokenstick`. This object collects the fitted broken stick model.
#'
#' The package exports S3 methods for the `brokenstick` class for the following
#' generic functions: [fitted()], [model.frame()], [model.matrix()], [plot()],
#' [predict()], [print()], [residuals()].
#'
#' The package documents methods [fitted.brokenstick()], [plot.brokenstick()],
#' [predict.brokenstick()] and [residuals.brokenstick()]. The package exports
#' two helper functions for `brokenstick` objects: [get_knots()] and [get_r2()].
#'
#' A `brokenstick` object is a list with the following named elements:
#'
#' @section Elements:
#' \describe{
#'    \item{`names`}{A list with elements named `x`, `y` and `g` providing the
#'    variables names for the time, outcome and subject columns, respectively.}
#'    \item{`knots`}{Numeric vector of with the internal knots. Use [get_knots()]
#'    to extract knots.}
#'    \item{`boundary`}{Numeric vector of length 2 with the boundary knots. Use
#'    [get_knots()] to extract knots.}
#'    \item{`degree`}{The `degree` of the B-spline. See [splines::bs()]. Either
#'    0 (constant model) or 1 (broken stick model).}
#'    \item{`model`}{Not used.}
#'    \item{`method`}{Either `"kr"` or `"lmer"`, identifying the fitting model.}
#'    \item{`control`}{List of control options returned by [set_control()].}
#'    \item{`beta`}{Numeric vector with fixed effect estimates.}
#'    \item{`omega`}{Numeric matrix with variance-covariance estimates of the
#'    broken stick estimates.}
#'    \item{`sigma2j`}{Numeric vector with estimates of the residual variance per
#'    group. Only used by method `"kr"`.}
#'    \item{`sigma2`}{Numeric scalar with the mean residual variance.}
#'    \item{`draws`}{Numeric matrix with multiple imputations. The number of
#'    rows is equal to the number of missing values in `y`. The number of columns
#'    depends on `imp_skip`. Only used by `kr` if `imp_skip` is set.}
#' }
#'
#' @name brokenstick-class
#' @rdname brokenstick-class
#' @author Stef van Buuren, 2020
#' @references van Buuren S (2020).
#' Broken Stick Model for Irregular Longitudinal Data. \emph{In preparation}.
NULL
