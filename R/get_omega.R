#' Extract Variance and Correlation Components
#'
#' Extracts variance-covariance or correlation matrix from a
#' `brokenstick` object.
#'
#' @param x      Object of class `brokenstick`
#' @param what   Either `"cov"` (default) for the covariance matrix,  or `"cor"`
#'  for the correlation matrix.
#' @param names  A vector of column names of. If not specified, the function
#'  automatically drops the entries corresponding to the right boundary. Specify
#'  `names = "all"` to prevent dropping.
#' @return A numeric matrix, possibly with zero rows and columns if no names match
#' @examples
#' f1 <- brokenstick(hgt_z ~ age | id, smocc_200[1:1000, ], knots = 0:3, seed = 1)
#' get_omega(f1, "cor", c("age_1", "age_2"))
#' @export
get_omega <- function(x, what = c("cov", "cor"), names = NULL) {
  stopifnot(inherits(x, "brokenstick"))
  what <- match.arg(what)
  omega <- x$omega
  if (length(names) == 1L && names == "all") {
    names <- colnames(omega)
  }
  if (is.null(names)) {
    names <- colnames(omega)[1L:length(colnames(omega)) - 1L]
  }
  names <- intersect(names, colnames(omega))
  if (length(names)) {
    omega <- omega[names, names, drop = FALSE]
  } else {
    omega <- matrix(NA_real_, 0L, 0L)
  }
  if (what == "cov") return(omega)
  if (dim(omega)[1L]) return(cov2cor(omega))
  return(omega)
}
