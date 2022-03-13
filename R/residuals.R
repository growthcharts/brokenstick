#' Extract residuals from brokenstick model
#'
#' @aliases residuals.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return
#' A numerical vector with residuals The number of elements equals the
#' number of rows in `newdata`. If `newdata` is not specified, the function
#' looks for the training data in `object` as the element named `data`.
#' @family brokenstick
#' @export
residuals.brokenstick <- function(object, newdata = NULL, ...) {
  newdata <- get_newdata(object, newdata)
  return(newdata[[object$names$y]] - fitted(object, newdata))
}
