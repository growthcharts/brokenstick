#' Kasim-Raudenbush sampler for two-level normal model
#'
#' Simulates posterior distributions of parameters from a two-level
#' normal model with heterogeneous within-cluster variances
#' (Kasim and Raudenbush, 1998). Imputations can be drawn as an
#' extra step to the algorithm.
#'
#' @inheritParams stats::lm
#' @param ... Other named arguments.
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#' @note Added June 25, 2012: The currently implemented algorithm does not
#' handle predictors that are specified as fixed effects (type=1). When using
#' \code{mice.impute.2l.norm()}, the current advice is to specify all predictors
#' as random effects (type=2).
#'
#' Warning: The assumption of heterogeneous variances requires that in every
#' class at least one observation has a response in \code{y}.
#' @author Roel de Jong, 2008
#' @references
#'
#' Kasim RM, Raudenbush SW. (1998). Application of Gibbs sampling to nested
#' variance components models with heterogeneous within-group variance. Journal
#' of Educational and Behavioral Statistics, 23(2), 93--116.
#'
#' Van Buuren, S. (2011) Multiple imputation of multilevel data. In Hox, J.J.
#' and and Roberts, J.K. (Eds.), \emph{The Handbook of Advanced Multilevel
#' Analysis}, Chapter 10, pp. 173--196. Milton Park, UK: Routledge.
kr <- function(data,
               formula,
               control = list(run_in = 100),
               subset,
               weights,
               na.action = na.action,
               ...) {
  new_brokenstick(data = data, formula = formula)
}
