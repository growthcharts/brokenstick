#' Export the estimates of a fitted brokenstick model
#' 
#' Exports the crucial estimates of a fitted brokenstick model so that the 
#' stored estimates can be used by the EB() function 
#' to calculate random effect estimates for new data.
#' 
#' @param model An object of class \code{brokenstick} or class 
#' \code{brokenstick_export}
#' @return A \code{list} of class \code{brokenstick_export}, with elements corresponding to the estimates parameters of the fitted model.
#' @export
export <- function(model) {

  # if already a broken.stick.export object, do nothing
  if (inherits(model, "brokenstick_export")) return(model)

  if (!inherits(model, "brokenstick"))
    stop("Argument 'model' expected as class 'brokenstick'")

  # extract estimates from merMod object
  beta <- fixef(model)
  # get variance of RE, Q*Q
  omega <- as.matrix(as.data.frame(VarCorr(model)$subject))
  df <- as.data.frame(VarCorr(model))
  sigma2 <- df[df$grp == "Residual", "vcov"]

  z <- list(beta = beta, omega = omega, sigma2 = sigma2,
            knots = attr(model, "knots"),
            boundary = attr(model, "boundary"),
            degree = attr(model, "degree"))

  class(z) <- "brokenstick_export"
  return(z)
}
