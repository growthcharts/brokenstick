#' Export the estimates of a fitted brokenstick model
#'
#' Exports the crucial estimates of a fitted brokenstick model so that the
#' stored estimates can be used by the EB() function
#' to calculate random effect estimates for new data.
#'
#' @param object An object of class \code{brokenstick} or class
#' \code{brokenstick_export}
#' @return A \code{list} of class \code{brokenstick_export}, with elements corresponding to the estimates parameters of the fitted model.
#' @export
export <- function(object) {

  # if already a broken.stick.export object, do nothing
  if (inherits(object, "brokenstick_export")) {
    return(object)
  }

  if (!inherits(object, "brokenstick")) {
    stop("Argument 'object' expected as class 'brokenstick'")
  }

  model <- object$model

  # extract estimates from merMod object
  beta <- lme4::fixef(model)
  # get variance of RE, Q*Q
  omega <- as.matrix(as.data.frame(VarCorr(model)[[object$names$z]]))
  df <- as.data.frame(VarCorr(model))
  sigma2 <- df[df$grp == "Residual", "vcov"]

  z <- list(
    beta = beta,
    omega = omega,
    sigma2 = sigma2,
    sigma2j = NA,
    knots = object$knots,
    boundary = object$boundary,
    degree = object$degree
  )

  class(z) <- "brokenstick_export"
  return(z)
}
