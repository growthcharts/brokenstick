#' Calculate fitted values
#'
#' @aliases fitted.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return See [predict.brokenstick()].
#' @family brokenstick
#' @export
fitted.brokenstick <- function(object, new_data = NULL, ...) {
  return(predict(object, new_data))
}
