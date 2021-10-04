#' @export
model.frame.brokenstick <- function(formula, ...) {
  return(formula$data)
}

#' @export
model.matrix.brokenstick <- function(object, ...) {
  x_name <- object$names$x[1L]
  obj <- make_basis(x = object$data[[x_name]],
                    knots = object$knots,
                    boundary = object$boundary)
  return(obj)
}
