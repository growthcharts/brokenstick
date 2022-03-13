#' @export
model.frame.brokenstick <- function(formula, data = NULL, ...) {
  if (formula$light) {
    return(NULL)
  }
  names <- unlist(formula$names)
  if (is.null(data)) data <- formula$data
  ff <- paste(names[["y"]], "~", names[["x"]], "+", names[["g"]])
  form <- as.formula(ff)
  return(model.frame.default(formula = form, data = data, ...))
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
