#' Extract Variance and Correlation Components
#'
#' Extracts variance-covariance or correlation matrix from a
#' `brokenstick` object.
#'
#' @param x      Object of class `brokenstick`
#' @param scale  Either `"cov"` (default) for the covariance matrix,  or `"cor"`
#'  for the correlation matrix.
#' @param \dots  Not used.
#' @export
VarCorr.brokenstick <- function(x, scale = c("cov", "cor"), ...) {
  scale <- match.arg(scale)
  if (scale == "cov") return(x$omega)
  return(cov2cor(x$omega))
}
