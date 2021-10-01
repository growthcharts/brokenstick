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
#' @param seed Seed number for [base::set.seed()]. Use `NA` to bypass
#' seed setting.
#' @return A list with components:
#'
#'     * `beta`  Fixed effects
#'     * `omega` Variance-covariance of random effects
#'     * `sigma2_j` Residual variance per group
#'     * `sigma2` Average residual variance
#'     * `draws` A matrix with `ndraw` columns with draws for missing data
#'
#' @author Stef van Buuren, based on [mice::mice.impute.2l.norm()]
#'
#' @details
#' The speed of the Kasim-Raudenbush sampler is almost
#' independent of the number of random effect, and foremost depends
#' on the *total number of iterations*: `end` in the `control` list.
#'
#' The defaults `start = 101`, `end = 300` and `thin = 1`
#' provide 200 draws with a *reasonable* approximation to the variance-covariance
#' matrix of the random effects.
#'
#' For a closer approximations with 200 draws set `end = 2100` with
#' `thin = 10` (*better*) or `end = 4100` with `thin = 20` (*best*),
#' at the expense of a linear increase in calculation time. Drawing fewer
#' than 50 observations is not recommended, and such results are best treated
#' as *indicative*.
#'
#' It is possible to draw multiple imputations for a subset of parameter draws.
#' The `thin_imp` should be specified as a multiple of `thin`. For example,
#' if `thin_imp` is 10 times `thin`, then the procedure will calculate and
#' store multiple imputations in every tenth of the parameter draws. Thus,
#' for 200 parameter draws there will be 20 draws from the posterior
#' distribution of the outcome variable.
#'
#' The total number of parameter draws equals `(end - start + 1) / thin`. The number
#' of multiple imputations equals `(end - start + 1) / thin_imp`. By default,
#' `thin_imp` is `Inf` and does not produce imputations.
#'
#' @references
#' Kasim RM, Raudenbush SW. (1998). Application of Gibbs sampling to nested
#' variance components models with heterogeneous within-group variance. Journal
#' of Educational and Behavioral Statistics, 23(2), 93--116.
#' @export
kr <- function(y,
               x,
               g,
               control,
               seed) {

  if (!is.na(seed)) set.seed(seed)

  ry <- !is.na(y)
  g <- as.integer(factor(g))  # convert character into integer
  xg <- cbind(x, g)
  type <- c(rep(2L, ncol(x)), -2L)

  res <- kr_vector(y, ry, xg, type, intercept = FALSE, control = control)
  dimnames(res$omega) <- list(colnames(x), colnames(x))
  obj <- list(beta = res$beta,
              omega = res$omega,
              sigma2j = res$sigma2j,
              sigma2 = res$sigma2,
              draws = res$draws)
  class(obj) <- "kr"
  obj
}
