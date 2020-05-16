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
#' @aliases brokenstick
#' @param y a vector containing the measurements to be analyzed
#' @param x a vector of length \code{length(y)} with the explanatory variable on which
#' the break points should be defined. In longitudinal data, this is usually age.
#' @param subjid a vector length \code{length(y)} containing the subject identification
#' @param k optional, scalar indicating the number of internal knots. If specified, then
#' \code{k} internal knots are placed at equidense quantiles of \code{x}. For example,
#' specifying \code{k = 1} puts a knot at the 50th quantile (median), specifying \code{k = 3} puts knots
#' at the 25th, 50th and 75th quantiles of \code{x}, and so on. If both \code{k} and
#' \code{knots} are specified, then \code{k} take precendence. Note that knots specified
#' via \code{k} are data-dependent and do not transfer well to other data sets. Use \code{knots}
#' to specify knots that are independent of the data \code{x}.
#' @param knots optional, numerical vector with the locations of the breaks to be
#' placed on the values of \code{x}. Be careful with values outside the range
#' of the data since this extends the \code{boundary} knots (see below) beyond
#' the data range.
#' @param boundary optional, but recommended. Numerical vector of length 2 with the minimum and maximum
#' knot. This \code{boundary} setting is passed to \code{splines::bs()} as the
#' \code{Boundary.knots} argument. If not specified, then the range of \code{x}
#' is taken. If \code{knots} is specified, then the boundary range is extended
#' to include at least \code{range{knots}}.
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
#'
#' # fit with implicit boundary c(0, 3)
#' fit <- with(data, brokenstick(y = hgt.z, x = age, subjid = subjid, knots = 0:3))
#' @note
#' The \code{storeX} and \code{degree} arguments have been deprecated in
#' version 0.54.
#' @export
brokenstick <- function(y, x, subjid,
                        k = NULL,
                        knots = NULL,
                        boundary = NULL,
                        method = c("lmer", "kr"),
                        control = lmerControl(check.nobs.vs.nRE = "warning"),
                        na.action = na.exclude,
                        ...) {
  call <- match.call()
  method <- match.arg(method)

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x = x, knots = l$knots, boundary = l$boundary)
  data <- data.frame(subjid = subjid, x = x, y = y, X)

  if (method == "lmer") {
    pred <- paste("0 +", paste(colnames(X), collapse = " + "))
    f <- as.formula(paste(
      "y", "~", pred,
      "+ (", pred, "| subjid)"
    ))
    model <- lmer(f,
                  data = data,
                  control = control,
                  na.action = na.action,
                  ...
    )

    fit <- list(
      model = model,
      knots = as.numeric(l$knots),
      boundary = as.numeric(l$boundary),
      degree = 1,
      bs.call = call,
      xy = data[, 1:3]
    )
    class(fit) <- c("brokenstick")
  }
  else
  {# kr sampler
    model <- kr(y = data$y,
                ry = !is.na(data$y),
                x = data[, c("group", colnames(X))],
                type = c(-2, rep(2, ncol(X))),
                intercept = FALSE,
                ...)
    fit <- list(
      model = NA,
      beta = model$mu,
      omega = solve(model$inv.psi),
      sigma2 = mean(1/model$inv.sigma2),
      sigma2j = 1/model$inv.sigma2,
      knots = as.numeric(l$knots),
      boundary = as.numeric(l$boundary),
      degree = 1,
      bs.call = call,
      xy = data[, 1:3])
    class(fit) <- "brokenstick"
  }
  fit
}

print.brokenstick <- function(x, ...) {
  cat("broken stick model \n")
  cat("knots: ", get_knots(x), "\n")
  print(summary(x))
  invisible(x)
}

