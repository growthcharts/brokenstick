#' Empirical Bayes predictor for random effects
#'
#' This function can estimate random effect for a given set of
#' model estimates and new user data. The unit may be
#' new to the model. The methods implements the
#' EB estimate (also known as BLUP)
#' as described in Skrondral and Rabe-Hasketh, 2009, p. 683.
#' This function can also provide the broken stick estimate for a given level,
#' the sum of the global (fixed) and individual (random) effects.
#' The current implementation does not provide prediction errors.
#'
#' @aliases EB
#' @param model An object of class \code{brokenstick.export}.
#' @param y A vector of new measurements for unit j, scaled in the same metric as the fitted model.
#' @param X A \code{nj * p} matrix with fixed effects for unit j, typically produced by \code{bs()}.
#' @param Z A \code{nj * q} matrix with random effects for unit j. The default sets \code{Z} equal to \code{X}.
#' @param BS A logical indicating whether broken stick estimates should be
#' returned (\code{BS = TRUE}) or the random effects (\code{BS = FALSE}).
#' The default is \code{TRUE}.
#' @return A vector of length q containing the random effect or broken stick  estimates for unit j.
#' @author Stef van Buuren, 2015
#' @references
#' Skrondal, A., Rabe-Hesketh, S. (2009).
#' Prediction in multilevel generalized linear models.
#' J. R. Statist. Soc. A, 172, 3, 659-687.
#' @examples
#' #
#' # EB estimate random effect for child id 10001
#' model <- export(fit_206)
#' data <- get_xy(fit_206, ids = 10001)
#' y <- data$y
#' X <- make_basis(data$x, knots = model$knots,
#'  boundary = model$boundary)
#' EB(model, y, X)
#' @export
EB <- function (model, y, X, Z = X, BS = TRUE) {

  # X should be a matrix
  if (!is.matrix(X)) stop("Argument 'X' is not a matrix.")

  # make sure we get the exported model
  exp <- export(model)

  # eliminate missing outcomes
  select <- !(is.na(y) | is.na(X[, 1]))

    # if there are no valid values left, return the fixed effect
  # as broken stick estimates
  if (!any(select)) return(exp$beta)

  # get into shape for matrix multiplication
  # dimensions: y nj * 1; Z nj * q; X nj * p
  y <- matrix(y[select], ncol = 1)
  Z <- matrix(Z[select, ], ncol = dim(Z)[2])
  X <- matrix(X[select, ], ncol = dim(X)[2])
  beta <- matrix(exp$beta, ncol = 1)

  # construct appropriate matrices
  sigma.inv <- solve(Z %*% exp$omega %*% t(Z) +
                       diag(exp$sigma2, nrow(Z)))

  # calculate random effect by EB estimate
  re <- exp$omega %*% t(Z) %*% sigma.inv %*% (y - X %*% beta)

  # calculate broken stick estimate by summing fixed and random parts
  if (BS) re <- exp$beta + re

  return(as.vector(re))
}
