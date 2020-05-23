#' Kasim-Raudenbush sampler for two-level normal model
#'
#' Simulates posterior distributions of parameters from a two-level
#' normal model with heterogeneous within-cluster variances
#' (Kasim and Raudenbush, 1998). Imputations can be drawn as an
#' extra step to the algorithm.
#'
#' @param y Vector with outcome value
#' @param x Matrix with predictor value
#' @param g Vector with ID values
#' @param control A list with elements `runin`, `ndraws` and `skip`
#' @param na.action Not really used here
#' @return A list with results. see kr_vector
#' @references
#'
#' Kasim RM, Raudenbush SW. (1998). Application of Gibbs sampling to nested
#' variance components models with heterogeneous within-group variance. Journal
#' of Educational and Behavioral Statistics, 23(2), 93--116.
#'
#' Van Buuren, S. (2011) Multiple imputation of multilevel data. In Hox, J.J.
#' and and Roberts, J.K. (Eds.), \emph{The Handbook of Advanced Multilevel
#' Analysis}, Chapter 10, pp. 173--196. Milton Park, UK: Routledge.
kr <- function(y,
               x,
               g,
               control,
               na.action) {

  ry <- !is.na(y)
  xg <- cbind(x, g)
  type <- c(rep(2L, ncol(x)), -2L)

  f <- kr_vector(y, ry, xg, type, intercept = FALSE,
                 runin = control$runin,
                 ndraw = control$ndraw,
                 skip = control$skip)

  list(model = f,
       beta = f$beta,
       omega = f$omega,
       sigma2j = f$sigma2j,
       sigma2 = f$sigma2,
       draws = f$draws
  )
}
