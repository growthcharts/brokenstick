#' Fit a broken stick model to irregular data
#'
#' The broken stick model models an irregularly observed series
#' of measurement by scaling them onto a user-specified set of
#' 'ideal' ages. The model codes age by a series of linear B-splines.
#' Differences between persons are expressed by a random effect
#' pertaining to each knot. On the individual level, each
#' modeled growth curve connect straight lines that join at the
#' chosen break ages, and hence look like a 'broken stick'.
#'
#' @details
#' Relations over time are modeled by the variance-covariance
#' parameters of the random effects. Currently, this matrix is estimated
#' as unstructured by \code{lmer()} from the \code{lme4} package.
#' This estimate may be unstable if
#' the number of children is small relative to the number of specified
#' knots.
#'
#' This function can be time consuming for data sets with thousands of
#' children.
#' @param data a \code{data.frame} with the data
#' @inheritParams stats::lm
#' @param knots optional, numerical vector with the locations of the breaks to be
#' placed on the values of \code{x}. Be careful with values outside the range
#' of the data since this extends the \code{boundary} knots (see below) beyond
#' the data range.
#' @param boundary optional, but recommended. Numerical vector of length 2 with the minimum and maximum
#' knot. This \code{boundary} setting is passed to \code{splines::bs()} as the
#' \code{Boundary.knots} argument. If not specified, then the range of \code{x}
#' is taken. If \code{knots} is specified, then the boundary range is extended
#' to include at least \code{range{knots}}.
#' @param k optional, scalar indicating the number of internal knots. If specified, then
#' \code{k} internal knots are placed at equidense quantiles of \code{x}. For example,
#' specifying \code{k = 1} puts a knot at the 50th quantile (median), specifying \code{k = 3} puts knots
#' at the 25th, 50th and 75th quantiles of \code{x}, and so on. If both \code{k} and
#' \code{knots} are specified, then \code{k} take precendence. Note that knots specified
#' via \code{k} are data-dependent and do not transfer well to other data sets. Use \code{knots}
#' to specify knots that are independent of the data \code{x}.
#' @param method Either \code{"kr"} or \code{"lmer"}
#' @param control A list with argument that can be used to control the
#' fitting of \code{lmer()}. The default
#' is set to \code{lmerControl(check.nobs.vs.nRE = "warning")}, which turn
#' fatal errors with respect the number of parameters into warnings.
#' @param na.action The function to call for the \code{na.action} argument
#' in \code{lmer()}. The default is \code{na.exclude}.
#' @param \dots Additional arguments passed down to \code{lmer()}
#' (e.g. to specify additional \code{lmer()} options.
#' @return A fitted model of class \code{brokenstick}, which extends the
#'  class \code{lmerMod}
#' @examples
#' data <- brokenstick::smocc_200
#' # fit <- fit_brokenstick(data, hgt.z ~ age | subjid, 0:2)
#' mf <- fit_brokenstick(smocc_200, hgt.z ~ age | subjid, knots = 0:2)
#'
#' @export
fit_brokenstick <- function(data,
                            formula,
                            knots = NULL,
                            boundary = NULL,
                            k = NULL,
                            subset = NULL,
                            weights = NULL,
                            na.action = na.exclude,
                            method = c("lmer", "kr", "model.frame"),
                            control = list(),
                            ...) {
  cl <- mc <- match.call()
  method <- match.arg(method)

  # parse formula
  mt <- match(c("formula", "data", "subset", "weights", "na.action",
                "offset"), names(mc), 0L)
  mc <- mc[c(1L, mt)]
  mc[[1]] <- quote(lme4::lFormula)
  lmod <- eval(mc, parent.frame(1L))
  if (method == "model.frame")
    return(lmod)
  nm <- names(lmod$fr)

  # redefine data with B-splines
  data <- lmod$fr
  y_name <- nm[1]
  x_name <- nm[2]
  z_name <- nm[length(nm)]
  y <- data[, y_name, drop = TRUE]
  x <- data[, x_name, drop = TRUE]
  z <- data[, z_name, drop = TRUE]

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x = x, knots = l$knots, boundary = l$boundary,
                  knotnames = TRUE)
  colnames(X) <- paste(x_name, colnames(X), sep = "_")
  data <- data.frame(y, x, z, X, stringsAsFactors = FALSE)
  names(data) <- c(y_name, x_name, z_name, colnames(X))

  pred <- paste("0 +", paste(colnames(X), collapse = " + "))
  fm <- as.formula(paste(y_name, "~", pred, "+ (", pred, "|", z_name, ")"))

  if (method == "lmer") {
    if (!length(control))
      control <- lmerControl(check.nobs.vs.nRE = "warning")
    model <- lmer(data = data,
                  formula = fm,
                  control = control,
                  subset = subset,
                  weights = weights,
                  na.action = na.action,
                  ...)
    df <- as.data.frame(VarCorr(model))
    fit <- list(
      call = cl,
      data = data,
      model = model,
      knots = as.numeric(l$knots),
      boundary = as.numeric(l$boundary),
      degree = 1L,
      beta = lme4::fixef(model),
      omega = as.matrix(as.data.frame(VarCorr(model)[[z_name]])),
      sigma2j = NA,
      sigma2 = df[df$grp == "Residual", "vcov"]
    )
    class(fit) <- c("brokenstick")
  }

  if (method == "kr") {
    model <- kr(data = data,
                formula = fm,
                control = control,
                na.action = na.action,
                ...)
    fit <- list(
      model = NA,
      beta = model$mu,
      omega = solve(model$inv.psi),
      sigma2 = mean(1/model$inv.sigma2),
      sigma2j = 1/model$inv.sigma2,
      knots = as.numeric(l$knots),
      boundary = as.numeric(l$boundary),
      degree = 1L,
      call = cl,
      xy = df)
    class(fit) <- "brokenstick"
  }
  fit
}

#' @export
print.brokenstick <- function(x, ...) {
  cat(paste0("Class: brokenstick (", class(x$model),")\n"))
  cat("Knots:", get_knots(x, "all"), "\n")
  cat("Means:", x$beta, "\n")
  cat("Variance-covariance matrix:\n")
  print(x$omega)
  if (!is.na(x$sigma2j)) cat("Cluster residuals: ", x$sigma2j, "\n")
  cat("Residual variance: ", x$sigma2, "\n")
  invisible(x)
}

#' @export
fitted.brokenstick <- function(x, ...) {
  predict(x, at = "knots", output = "broad")
}
