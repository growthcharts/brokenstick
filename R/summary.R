#' @inheritParams get_omega
#' @export
summary.brokenstick <- function(object, ...,
                                digits = max(3, getOption("digits")),
                                what = c("cov", "cor")) {
  stopifnot(inherits(object, "brokenstick"))
  what <- match.arg(what)

  ans <- list()
  ans$names <- object$names
  ans$knots <- get_knots(object, "all")
  ans$control <- object$control
  ans$model <- localsummary.model(object)
  ans$method <- object$method
  ans$beta <- localsummary.beta(object, digits)
  ans$omega <- localsummary.omega(object, digits, what = what)
  ans$sigma2j <- summary(object$sigma2j)[c(1:3, 5, 6)]
  ans$sigma2 <- object$sigma2
  ans$light <- object$light
  ans$sample <- object$sample
  if (!object$light) {
    ans$r2 <- get_r2(object, object$data)
  }
  ans$what <- what
  class(ans) <- "summary.brokenstick"
  return(ans)
}

#' @export
print.summary.brokenstick <- function(x,
                                      digits = max(3L, getOption("digits") - 3L),
                                      ...) {
  stopifnot(inherits(x, "summary.brokenstick"))
  cat(paste0("Class        brokenstick (", x$method, ")"))
  if (x$light) cat(" light")
  cat("\n")
  cat("Variables   ", x$names$y, "(outcome),", x$names$x, "(predictor),", x$names$g, "(group)\n")
  cat("Data        ", x$sample[1L], "(n),", x$sample[3], "(nmis),", x$sample[4], "(groups)\n")
  cat("Parameters  ", x$model$npar, "(total),", x$model$nfixed,"(fixed),",
      x$model$nvar, "(variance),", x$model$ncov, "(covariance),",
      x$model$nerr, "(error)\n")
  cat("Knots       ", x$knots, "\n")
  cat("Means       ", x$beta, "\n")
  if (length(x$sigma2j))
    cat("Residuals   ", x$sigma2j, "(min, P25, P50, P75, max)\n")
  cat("Mean resid  ", x$sigma2, "\n")
  if (!is.null(x$r2)) cat("R-squared   ", x$r2, "\n")
  cat("\n")
  if (x$what == "cov") cat("Variance-covariance matrix\n")
  if (x$what == "cor") cat("Correlation matrix\n")
  print(x$omega)
  cat("\n")
  return(invisible(x))
}

# summary helpers
localsummary.model <- function(x) {
  k <- length(get_knots(x, "all")) - 1
  if (x$method == "kr") {
    mdl <- list(
      model = "kr",
      npar = (k^2 + 5 * k + 6)/2 + 1,
      nfixed = k + 1,
      nvar = k + 1,
      ncov = (k + 1) * k / 2,
      nerr = 2L,
      nimp = x$control$nimp
    )
  }
  if (x$method == "lmer") {
    mdl <- list(
      model = "lmer",
      npar = (k^2 + 5 * k + 6) / 2,
      nfixed = k + 1,
      nvar = k + 1,
      ncov = (k + 1) * k / 2,
      nerr = 1L,
      nimp = 0L
    )
  }
  return(mdl)
}

localsummary.beta <- function(x, digits) {
  return(round(x$beta, digits))
}

localsummary.omega <- function(x, digits, what) {
  omega <- get_omega(x, what = what)
  upper <- round(omega, digits)
  upper[upper.tri(upper)] <- ""
  upper <- as.data.frame(upper)
  return(upper)
}
