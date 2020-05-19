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

  z <- list(
    beta = object$beta,
    omega = object$omega,
    sigma2j = object$sigma2j,
    sigma2 = object$sigma2,
    knots = object$knots,
    boundary = object$boundary,
    degree = object$degree,
    names = object$names
  )

  class(z) <- "brokenstick_export"
  z
}
