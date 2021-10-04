#' @export
print.brokenstick <- function(x, ..., digits = 2L) {
  cat(paste0("Class: brokenstick (", x$method,")\n"))
  cat("Knots:", get_knots(x, "all"), "\n")
  cat("Means:", round(x$beta, digits), "\n")
  cat("Variance-covariance matrix:\n")
  upper <- round(x$omega, digits)
  upper[upper.tri(upper)] <- ""
  upper <- as.data.frame(upper)
  print(upper)
  if (length(x$sigma2j)) cat("Cluster residuals (min, P25, P50, P75, max): ",
                             summary(x$sigma2j)[c(1:3, 5, 6)], "\n")
  cat("Residual variance: ", x$sigma2, "\n")
  return(invisible(x))
}
