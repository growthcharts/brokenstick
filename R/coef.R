#' @export
coef.brokenstick <- function(object, complete = TRUE, ...) {
  cf <- object$beta
  if (complete) {
    return(cf)
  } else {
    cf[!is.na(cf)]
  }
}
