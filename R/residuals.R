#' Extract residuals from brokenstick model
#'
#' @aliases residuals.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return A data.frame with a column named `.resid`
#' @family brokenstick
#' @export
residuals.brokenstick <- function(object, newdata = NULL, ...) {
  if (is.null(newdata) && object$light) {
    stop("A light brokenstick object expects a `newdata` argument.", call. = FALSE)
  }
  if (is.null(newdata) && !object$light) {
    newdata <- object$data
  }
  pred <- predict(object, newdata)
  y <- newdata[[object$names$y]]
  r <- y - pred
  colnames(r) <- ".resid"
  return(r)
}
