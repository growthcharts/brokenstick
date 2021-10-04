#' Extract residuals from brokenstick model
#'
#' @aliases residuals.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return See [predict.brokenstick()].
#' @family brokenstick
#' @export
residuals.brokenstick <- function(object, new_data = NULL, ...) {
  pred <- predict(object, new_data)
  y <- new_data[[object$names$y]]
  r <- y - pred
  colnames(r) <- ".resid"
  return(r)
}
