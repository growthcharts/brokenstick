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
#' @param knots a numerical vector with the locations of the breaks to be
#' placed on the values of \code{x}. This setting is passed to \code{bs()}. Specify \code{knots} to include the range of \code{x}. This will evade the warning
#' \code{some 'x' values beyond boundary knots may cause ill-conditioned bases}, a situation that may lead to nonsensical results. If in doubt, set argument \code{storeX = TRUE} and inspect the \code{X} slot of the result. The \code{X} matrix should have values between 0 and 1, and each row should add up to 1.
#' @param boundary a numerical vector of length 2 with the minimum and maximum
#' break point for \code{x}. This setting is passed to \code{bs()}. The default is to set the left boundary knot equal to the \code{min(knots)}. The right boundary knots is taken as the larger of \code{max(knots)} and the maximum of \code{x}.
#' @param degree the degree of the B-spline. For the broken stick model this
#' should be to 1 (the default), which specifies that values between the break
#' points are located on a straight line.
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
#' @export
brokenstick <- function(y, x, subjid,
                        knots = pretty(x),
                        boundary =
                          c(min(knots),
                            max(max(x, na.rm = TRUE, max(knots)))),
                        degree = 1,
                        control = lmerControl(check.nobs.vs.nRE = "warning"),
                        na.action = na.exclude,
                        ...) {
  call <- match.call()
  if (degree != 1) stop("No support for a degree different from 1.")
  X <- bs(x = x, knots = knots, Boundary.knots = boundary,
          degree = degree)
  colnames(X) <- paste("x", 1:ncol(X), sep = "")
  pred <- paste("0 +", paste(colnames(X), collapse = " + "))
  data <- data.frame(subjid = subjid, x = x, y = y, X)
  f <- as.formula(paste("y", "~", pred,
                        "+ (", pred, "| subjid)"))
  fit <- lmer(f, data = data,
              control = control,
              na.action = na.action,
              ...)

  class(fit) <- "brokenstick"
  fit@knots <- knots
  fit@boundary <- boundary
  fit@degree <- degree
  fit@bs.call <- call
  fit@xy <- data[, 1:3]
  return(fit)
}
