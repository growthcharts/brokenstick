#' Extract Model Coefficients from brokenstick Object
#'
#' @rdname coef
#' @param object A \code{brokenstick} object
#' @param hide Should output for boundary knots be hidden in the print,
#' summary and plot functions? Can be `"left"`, `"right"`, `"both"` or `"none"`.
#' If not specified, it is read from the field `object$hide`.
#' @param ... Not used
#' @inheritParams stats::coef
#' @export
coef.brokenstick <- function(object, complete = TRUE, ...,
                             hide = c("right", "left", "both", "none"))
{
  if (!missing(hide)) {
    hide <- match.arg(hide)
  } else {
    hide <- object$hide
  }

  beta <- object$beta
  beta <- switch(hide,
                 right = beta[-length(beta)],
                 left = beta[-1L],
                 both = beta[-c(1L, length(beta))],
                 none = beta)

  if (complete) {
    return(beta)
  } else {
    beta[!is.na(beta)]
  }
}
