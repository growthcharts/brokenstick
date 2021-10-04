#' Calculate fitted values
#'
#' @aliases fitted.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return See [predict.brokenstick()].
#' @family brokenstick
#' @export
fitted.brokenstick <- function(object, newdata = NULL, ...) {
  return(predict(object, newdata))
}
