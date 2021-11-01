#' Extract residuals from brokenstick model
#'
#' @aliases residuals.brokenstick
#' @inheritParams predict.brokenstick
#' @param \dots Additional arguments. Ignored.
#' @return A data.frame with a column named `.resid`
#' @family brokenstick
#' @export
residuals.brokenstick <- function(object, newdata = NULL, ...) {
  newdata <- get_newdata(object, newdata)
  pred <- predict(object, newdata)
  y <- newdata[[object$names$y]]
  r <- y - pred
  colnames(r) <- ".resid"
  return(r)
}
