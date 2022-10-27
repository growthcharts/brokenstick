#' @inheritParams get_omega
#' @export
summary.brokenstick <- function(object,
                                ...,
                                cor = FALSE,
                                lower = TRUE,
                                hide = c("right", "left", "boundary", "internal", "none")) {
  stopifnot(inherits(object, "brokenstick"))
  if (!missing(hide)) {
    hide <- match.arg(hide)
  } else {
    hide <- ifelse(is.null(object$hide), "right", object$hide)
  }

  ans <- list()
  ans$names <- object$names
  ans$knots <- get_knots(object, hide = hide)
  ans$control <- object$control
  ans$model <- localsummary.model(object)
  ans$method <- object$method
  ans$beta <- coef(object, hide = hide)
  omega <- get_omega(object, cor = cor, hide = hide)
  if (lower) omega[upper.tri(omega)] <- NA_real_
  ans$omega <- omega
  if (length(object$sigma2j)) {
    ans$sigma2j <- summary(object$sigma2j)[c(1:3, 5, 6)]
  } else {
    ans$sigma2j <- object$sigma2j
  }
  ans$sigma2 <- object$sigma2
  ans$light <- object$light
  ans$sample <- object$sample
  if (!object$light) {
    ans$r2 <- get_r2(object, object$data)
  }
  ans$hide <- hide
  ans$cor <- cor
  class(ans) <- "summary.brokenstick"
  return(ans)
}

# summary helpers
localsummary.model <- function(x) {
  k <- length(get_knots(x, hide = "none")) - 1
  if (x$method == "kr") {
    mdl <- list(
      model = "kr",
      npar = (k^2 + 5 * k + 6) / 2 + 1,
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
