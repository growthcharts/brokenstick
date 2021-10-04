#' Extract residuals from brokenstick model
#'
#' @aliases residuals.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return See [predict.brokenstick()].
#' @family brokenstick
#' @export
residuals.brokenstick <- function(object, newdata = NULL, ...) {
  pred <- predict(object, newdata)
  y <- newdata[[object$names$y]]
  r <- y - pred
  colnames(r) <- ".resid"
  return(r)
}
