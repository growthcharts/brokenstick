#' @export
model.frame.brokenstick <- function(formula, ...) {
  if (formula$light) {
    return(NULL)
  }
  return(formula$data)
}

#' @export
model.matrix.brokenstick <- function(object, ...) {
  if (object$light) {
    return(NULL)
  }
  x_name <- object$names$x[1L]
  obj <- make_basis(
    x = object$data[[x_name]],
    internal = get_knots(object, "internal"),
    boundary = get_knots(object, "boundary")
  )
  return(obj)
}
