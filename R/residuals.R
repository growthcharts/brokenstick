#' Extract residuals from brokenstick model
#'
#' @aliases residuals.brokenstick
#' @inheritParams predict_new.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return See [predict_new.brokenstick()].
#' @family brokenstick
#' @export
residuals.brokenstick <- function(object, new_data = NULL, ...) {
  pred <- predict_new(object, new_data)
  y <- new_data[[object$names$y]]
  r <- y - pred
  colnames(r) <- ".resid"
  r
}
