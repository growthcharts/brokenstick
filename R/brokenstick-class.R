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
#'    \item{`formula`}{A formula with the model specification,
#'    e.g. `formula(y ~ x | group)`}
#'    \item{`names`}{A list with elements named `x`, `y` and `g` providing the
#'    variables names for the time, outcome and subject columns, respectively.}
#'    \item{`knots`}{Numeric vector of with internal knots. Use [get_knots()]
#'    to extract knots.}
#'    \item{`boundary`}{Numeric vector of length 2 with the boundary knots. Use
#'    [get_knots()] to extract knots.}
#'    \item{`degree`}{The `degree` of the B-spline. See [splines::bs()]. Support
#'    only the values of 0 (step model) or 1 (broken stick model).}
#'    \item{`method`}{Either `"kr"` or `"lmer"`, identifying the fitting model.}
#'    \item{`control`}{List of control options returned by [set_control()] used
#'    to set algorithmic details.}
#'    \item{`beta`}{Numeric vector with fixed effect estimates.}
#'    \item{`omega`}{Numeric matrix with variance-covariance estimates of the
#'    broken stick estimates.}
#'    \item{`sigma2j`}{Numeric vector with estimates of the residual variance per
#'    group. Only used by method `"kr"`.}
#'    \item{`sigma2`}{Numeric scalar with the mean residual variance.}
#'    \item{`light`}{Should the returned object be lighter? If `light = TRUE`
#'    the returned object will contain only the model settings and parameter
#'    estimates and not store the `data`, `imp` and `mod` elements. The light
#'    object can be used to predict broken stick estimates for new data, but
#'    does not disclose the training data and is small.}
#'    \item{`data`}{The training data used to fit the model.}
#'    \item{`imp`}{The imputations generated for the missing outcome data. Only
#'    for `kr`.}
#'    \item{`mod`}{Either an object of class `kr` or [lme4::merMod-class],
#'    depending on `method`.}
#' }
#'
#' @name brokenstick-class
#' @rdname brokenstick-class
#' @author Stef van Buuren, 2021
#' @references van Buuren S (2021).
#' Broken Stick Model for Irregular Longitudinal Data. \emph{In preparation}.
NULL

new_brokenstick <- function(formula = formula(),
                            names = list(x = character(),
                                         y = character(),
                                         g = character()),
                            knots = numeric(0),
                            boundary = numeric(0),
                            degree = 1L,
                            method = NA_character_,
                            control = list(),
                            beta = numeric(0),
                            omega = numeric(0),
                            sigma2j = numeric(0),
                            sigma2 = numeric(0),
                            light = FALSE,
                            data = numeric(0),
                            imp = numeric(0),
                            mod = list()) {
  result <- list(formula = formula,
                 names = names,
                 knots = knots,
                 boundary = boundary,
                 degree = degree,
                 method = method,
                 control = control,
                 beta = beta,
                 omega = omega,
                 sigma2j = sigma2j,
                 sigma2 = sigma2,
                 light = light)
  if (!light) {
    result$data <- data
    result$imp <- imp
    result$mod <- mod
  }
  class(result) <- "brokenstick"
  return(result)
}
