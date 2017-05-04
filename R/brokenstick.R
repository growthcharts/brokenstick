# brokenstick.R
#

setClass("brokenstick",
         representation(knots = "numeric",
                        boundary = "numeric",
                        degree = "numeric",
                        bs.call = "call",
                        xy = "data.frame"),
         contains = "lmerMod")

# ==============================================================================
# S4 print function for brokenstick object
# ==============================================================================
setGeneric("print")
setMethod("print", signature( x = "brokenstick" ),
          function (x, ... ) {
            print_brokenstick(x, ...)
          }
)

print_brokenstick <- function (x, ... ) {
  cat("broken stick model \n")
  cat("knots: ", get_knots(x), "\n")
  print(summary(x))
  invisible(x)
}


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
#' @param boundary optional, numerical vector of length 2 with the minimum and maximum
#' knot. This \code{boundary} setting is passed to \code{splines::bs()} as the
#' \code{Boundary.knots} argument. If not specified, then the range of \code{x}
#' is taken. If \code{knots} is specified, then the boundary range is extended
#' to include at least \code{range{knots}}.
#' @param control A function to control fitting of \code{lmer()}. The default
#' is set to \code{lmerControl(check.nobs.vs.nRE = "warning")}, which turn
#' fatal errors with respect the number of parameters into warnings.
#' @param na.action The function to call for the \code{na.action} argument in \code{lmer()}. The default is \code{na.exclude}.
#' @param \dots Additional arguments passed down to \code{lmer()}
#' (e.g. to specify additional \code{lmer()} options.
#' @return A fitted model of class \code{brokenstick}, which extends the
#'  class \code{lmerMod}
#' @examples
#' library(mice)
#' data <- tbc[tbc$id < 1000 & tbc$age < 2.5,]
#' fit <- brokenstick(y = data$hgt.z, x = data$age, subjid = data$id,
#'                    knots = c(0, 1, 2))
#' @note
#' The \code{storeX} and \code{degree} arguments have been deprecated in
#' version 0.54.
#' @export
brokenstick <- function(y, x, subjid,
                        k = NULL,
                        knots = NULL,
                        boundary = NULL,
                        control = lmerControl(check.nobs.vs.nRE = "warning"),
                        na.action = na.exclude,
                        ...) {
  call <- match.call()

  k_orig <- k
  knots_orig <- knots
  boundary_orig <- boundary

  # if not specified, define boundary as data range
  range <- range(x, na.rm = TRUE)
  if (is.null(boundary_orig)) boundary <- range
  if (length(boundary_orig) != 2) boundary <- range

  # if knots is specified
  #   extend lower boundary to min(knots)
  #   extend upper boundary to max(knots)
  if (!is.null(knots_orig)) {
    boundary[1] <- min(min(knots_orig, na.rm = TRUE), boundary[1])
    boundary[2] <- max(max(knots_orig, na.rm = TRUE), boundary[2])
  }

  # set k to zero if not specified
  if (is.null(k_orig)) k <- 0

  # if there is vector input via knots and if no k specified
  # trim knots to exclude boundary points, and calculate k
  if (is.null(k_orig) & length(knots_orig) >= 1) {
    knots <- as.numeric(knots_orig)
    knots <- knots[knots > boundary[1] & knots < boundary[2]]
    k <- length(knots)
  }

  # for scalar k, calculate equidense quantiles from the data
  if (!is.null(k_orig)) {
    if (k_orig >= 0 & k_orig <= 25) {
      k <- k_orig
      knots <- quantile(x, probs = seq(0, 1, length.out = k + 2))[-c(1, k + 2)]
    }
    else
      stop("Number of knots outside range 0-25")
  }

  X <- make_basis(x = x, knots = knots, boundary = boundary)

  pred <- paste("0 +", paste(colnames(X), collapse = " + "))
  data <- data.frame(subjid = subjid, x = x, y = y, X)
  f <- as.formula(paste("y", "~", pred,
                        "+ (", pred, "| subjid)"))
  fit <- lmer(f, data = data,
              control = control,
              na.action = na.action,
              ...)

  class(fit) <- "brokenstick"
  fit@knots <- as.numeric(knots)
  fit@boundary <- as.numeric(boundary)
  fit@degree <- 1
  fit@bs.call <- call
  fit@xy <- data[, 1:3]
  return(fit)
}
