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
#' @param control A list with elements:
#'
#'    * `runin`: Number of run-in iterations
#'    * `ndraws`: Number of parameter draws
#'    * `par_skip`: Number of iterations to next parameter draw
#'    * `imp_skip`: Number of iterations to next outcome draw
#'
#' @param na.action Not really used here
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
#' The calculation time of [lme4::lmer()] rapidly increases with the
#' number of random effects. More than 10 random effects (knots)
#' takes significant time, and beyond 15 knots generally impossible
#' to fit.
#'
#' In contrast, the speed of the Kasim-Raudenbush sampler is almost
#' independent of the number of random effect, and foremost depends
#' on the *total number of iterations*: `runin` + `ndraws` * `par_skip`.
#'
#' The defaults `ndraws = 200` and `par_skip = 1` provides a *good*
#' approximation to the variance-covariance matrix of the random
#' effects. Increase `par_skip` to `10` (*better*) or `20` (*best*) to
#' obtain closer approximations at the expense of a linear
#' increase in calculation time. Setting `ndraws = 50` (or lower)
#' will reduce computation time, but the result should be treated as
#' *indicative*.
#'
#' It is possible to subsample the parameter draws at every
#' `imp_skip` iteration, and draw one or more synthetic outcome values
#' from the posterior distribution of the outcome. The number of
#' multiple imputations is equal to `floor(ndraws / imp_skip)`. Thus,
#' setting `imp_skip = 20L` returns 200 / 20 = 10 multiple
#' imputations for each missing value in the outcome. The default does
#' not produce imputations.
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
               na.action) {

  ry <- !is.na(y)
  xg <- cbind(x, g)
  type <- c(rep(2L, ncol(x)), -2L)

  res <- kr_vector(y, ry, xg, type, intercept = FALSE,
                 runin = control$runin,
                 ndraw = control$ndraw,
                 par_skip = control$par_skip,
                 imp_skip = control$imp_skip)

  obj <- list(beta = res$beta,
              omega = res$omega,
              sigma2j = res$sigma2j,
              sigma2 = res$sigma2,
              draws = res$draws)
  class(obj) <- "kr"
  obj
}
