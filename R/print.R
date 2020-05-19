#' @export
print.brokenstick <- function(x, ..., digits = 2L) {
  cat(paste0("Class: brokenstick (", class(x$model),")\n"))
  cat("Knots:", get_knots(x, "all"), "\n")
  cat("Means:", round(x$beta, digits), "\n")
  cat("Variance-covariance matrix:\n")
  upper <- round(x$omega, digits)
  upper[upper.tri(upper)] <- ""
  upper <- as.data.frame(upper)
  print(upper)
  if (!is.na(x$sigma2j)) cat("Cluster residuals: ", x$sigma2j, "\n")
  cat("Residual variance: ", x$sigma2, "\n")
  invisible(x)
}

#' @export
print.brokenstick_export <- function(x, ..., digits = 2L) {
  cat(paste0("Class: brokenstick_export\n"))
  cat("Names:", unname(unlist(x$names)), "\n")
  cat("Knots:", sort(unique(c(x$knots, x$boundary))), "\n")
  cat("Means:", round(x$beta, digits), "\n")
  cat("Variance-covariance matrix:\n")
  upper <- round(x$omega, digits)
  upper[upper.tri(upper)] <- ""
  upper <- as.data.frame(upper)
  print(upper)
  if (!is.na(x$sigma2j)) cat("Cluster residuals: ", x$sigma2j, "\n")
  cat("Residual variance: ", x$sigma2, "\n")
  invisible(x)
}
