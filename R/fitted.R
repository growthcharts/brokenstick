#' Calculate fitted values
#'
#' @aliases fitted.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return
#' A numerical vector with predictions. The number of elements equals the
#' number of rows in `newdata`. If `newdata` is not specified, the function
#' looks for the training data in `object` as the element named `data`.
#' @family brokenstick
#' @export
fitted.brokenstick <- function(object, newdata = NULL, ...) {
  return(predict(object, newdata, shape = "vector", include_data = FALSE))
}
