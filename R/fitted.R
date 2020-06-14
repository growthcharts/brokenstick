#' Calculate fitted values
#'
#' @aliases fitted.brokenstick
#' @inheritParams predict_new.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return See [predict_new.brokenstick()].
#' @family brokenstick
#' @export
fitted.brokenstick <- function(object, new_data = NULL, ...) {
  predict_new(object, new_data)
}
