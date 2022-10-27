#' @export
print.brokenstick <- function(x,
                              digits = getOption("digits"),
                              ...,
                              hide = c("right", "left", "boundary", "internal", "none")) {
  stopifnot(inherits(x, "brokenstick"))
  if (!missing(hide)) {
    hide <- match.arg(hide)
  } else {
    hide <- ifelse(is.null(x$hide), "right", x$hide)
  }

  cat(paste0("Class        brokenstick (", x$method, ")"))
  if (x$light) cat(" light")
  cat("\n")
  cat("Variables   ", x$names$y, "(outcome),", x$names$x, "(predictor),", x$names$g, "(group)\n")
  cat("Knots       ", format(get_knots(x, hide = hide), digits = digits, ...), "\n")
  cat("Mean resid  ", format(x$sigma2, digits = digits, ...), "\n")
  if (!is.null(x$r2)) cat("R-squared   ", format(x$r2, digits = digits, ...), "\n")
  return(invisible(x))
}

#' @export
print.summary.brokenstick <- function(x,
                                      digits = getOption("digits"),
                                      na.print = "",
                                      ...) {
  stopifnot(inherits(x, "summary.brokenstick"))
  cat(paste0("Class        brokenstick (", x$method, ")"))
  if (x$light) cat(" light")
  cat("\n")
  cat("Variables   ", x$names$y, "(outcome),", x$names$x, "(predictor),", x$names$g, "(group)\n")
  cat("Data        ", x$sample[1L], "(n),", x$sample[3], "(nmis),", x$sample[4], "(groups)\n")
  cat(
    "Parameters  ", x$model$npar, "(total),", x$model$nfixed, "(fixed),",
    x$model$nvar, "(variance),", x$model$ncov, "(covariance),",
    x$model$nerr, "(error)\n"
  )
  cat("Knots       ", format(x$knots, digits = digits, ...), "\n")
  cat("Means       ", format(x$beta, digits = digits, ...), "\n")
  if (length(x$sigma2j)) {
    cat("Residuals   ", format(x$sigma2j, digits = digits, ...), "(min, P25, P50, P75, max)\n")
  }
  cat("Mean resid  ", format(x$sigma2, digits = digits, ...), "\n")
  if (!is.null(x$r2)) cat("R-squared   ", format(x$r2, digits = digits, ...), "\n")
  cat("\n")
  if (x$cor) cat("Correlation matrix\n")
  else cat("Variance-covariance matrix\n")
  print(x$omega, digits = digits, na.print = na.print, ...)
  return(invisible(x))
}
