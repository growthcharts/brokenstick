#' Kasim-Raudenbush sampler for two-level normal model
#'
#' Simulates posterior distributions of parameters from a two-level
#' normal model with heterogeneous within-cluster variances
#' (Kasim and Raudenbush, 1998). Imputations can be drawn as an
#' extra step to the algorithm.
#'
#' @param y Vector with outcome value
#' @param x Matrix with predictor value
#' @param g Vector with group values
#' @param control A list created by [control_kr()]
#' @return A list with components:
#'
#'     * `beta`  Fixed effects
#'     * `omega` Variance-covariance of random effects
#'     * `sigma2_j` Residual variance per group
#'     * `sigma2` Average residual variance
#'     * `imputes` Numeric matrix with multiple imputations `m`. The number of
#'    rows is equal to the number of missing values in the outcome vector `y`.
#'    The number of columns equals `m`.
#'
#' @author Stef van Buuren, based on [mice::mice.impute.2l.norm()]
#'
#' @details
#' The speed of the Kasim-Raudenbush sampler is almost
#' independent of the number of random effect, and foremost depends
#' on the *total number of iterations*.
#'
#' The defaults `start = 100`, `n = 200` and `thin = 1` provide 200 parameter
#' draws with a *reasonable* approximation to the variance-covariance
#' matrix of the random effects.
#'
#' For a closer approximations with 200 draws set `control = control_kr(thin = 10)`
#' (*better*) or `thin = 20` (*best*), at the expense of a linear increase in calculation
#' time. Drawing fewer than 50 observations is not recommended, and such
#' results are best treated as *indicative*.
#'
#' It is possible to draw multiple imputations by setting the `m` parameters.
#' For example, to draw five imputations for each missing outcome specify
#' `control = control_kr(m = 5)`.
#'
#' @references
#' Kasim RM, Raudenbush SW. (1998). Application of Gibbs sampling to nested
#' variance components models with heterogeneous within-group variance. Journal
#' of Educational and Behavioral Statistics, 23(2), 93--116.
#' @export
kr <- function(y,
               x,
               g,
               control) {

  if (!is.na(control$seed)) set.seed(control$seed)

  ry <- !is.na(y)
  g <- as.integer(factor(g))  # convert character into integer
  xg <- cbind(x, g)
  type <- c(rep(2L, ncol(x)), -2L)

  res <- kr_vector(y, ry, xg, type, intercept = FALSE, control = control)

  # fold triangular vector into var-cov matrix
  omega <- matrix(0, ncol(x), ncol(x))
  omega[lower.tri(omega, diag = TRUE)] <- res$omega
  omega[upper.tri(omega)] <- t(omega)[upper.tri(t(omega))]
  row.names(omega) <- colnames(omega) <- colnames(x)
  obj <- list(beta = res$beta,
              omega = omega,
              sigma2j = res$sigma2j,
              sigma2 = res$sigma2,
              imp = res$imputes,
              mod = res$mcmc)
  class(obj) <- "kr"
  return(obj)
}
