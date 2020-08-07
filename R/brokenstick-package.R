#' @importFrom dplyr           bind_rows bind_cols filter group_by
#'                             mutate pull relocate select ungroup `%>%`
#' @importFrom lme4            fixef lmer lmerControl ranef VarCorr
#' @importFrom methods         slot
#' @importFrom matrixsampling  rwishart
#' @importFrom rlang           .data arg_match
#' @importFrom splines         bs
#' @importFrom stats           approx as.formula cor cov2cor
#'                             fitted model.frame lm
#'                             model.matrix na.exclude na.omit
#'                             optim
#'                             predict quantile rgamma rnorm
#'                             rWishart setNames smooth var
#' @importFrom tidyr           drop_na pivot_wider
#' @importFrom utils           askYesNo install.packages
NULL

#' \pkg{brokenstick}: A package for irregular longitudinal data.
#'
#' The broken stick model describes a set of individual curves
#' by a linear mixed model using first order linear B-splines. The
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
#'   \code{predict()} \tab Obtain predictions from fitted model\cr
#'   }
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
