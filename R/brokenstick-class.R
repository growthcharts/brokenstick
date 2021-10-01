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
#'    \item{`method`}{Either `"kr"` or `"lmer"`, identifying the fitting model.}
#'    \item{`control`}{List of control options returned by [set_control()].}
#'    \item{`beta`}{Numeric vector with fixed effect estimates.}
#'    \item{`omega`}{Numeric matrix with variance-covariance estimates of the
#'    broken stick estimates.}
#'    \item{`sigma2j`}{Numeric vector with estimates of the residual variance per
#'    group. Only used by method `"kr"`.}
#'    \item{`sigma2`}{Numeric scalar with the mean residual variance.}
#'    \item{`imps`}{Numeric matrix with multiple imputations `m`. The number of
#'    rows is equal to the number of missing values in the outcome vector `y`.
#'    The number of columns equals `m`. Only created by `"kr"`.}
#'    \item{`mcmc`}{A list with `mcmc` objects with the history of parameter
#'    draws from the Kasim-Raudenbush sampler}
#' }
#'
#' @name brokenstick-class
#' @rdname brokenstick-class
#' @author Stef van Buuren, 2021
#' @references van Buuren S (2021).
#' Broken Stick Model for Irregular Longitudinal Data. \emph{In preparation}.
NULL

new_brokenstick <- function(names = list(x = character(),
                                         y = character(),
                                         z = character()),
                            knots = numeric(0),
                            boundary = numeric(0),
                            degree = 1L,
                            method = NA_character_,
                            control = list(),
                            beta = numeric(0),
                            omega = numeric(0),
                            sigma2j = numeric(0),
                            sigma2 = numeric(0),
                            imps = numeric(0),
                            mcmc = list()) {
  result <- list(names = names,
                 knots = knots,
                 boundary = boundary,
                 degree = degree,
                 method = method,
                 control = control,
                 beta = beta,
                 omega = omega,
                 sigma2j = sigma2j,
                 sigma2 = sigma2,
                 imps = imps,
                 mcmc = mcmc)
  class(result) <- "brokenstick"
  result
}
