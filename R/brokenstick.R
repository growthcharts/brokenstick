# brokenstick.R
# 

setClass("brokenstick",
         representation(knots = "numeric",
                        Boundary.knots = "numeric",
                        degree = "numeric",
                        X = "ANY"),
         contains = "lmerMod")

# ==============================================================================
# S4 print function for brokenstick object
# ==============================================================================
setGeneric("print")
setMethod("print", signature( x = "brokenstick" ),
          function ( x, ... ) {
            print.brokenstick( x, ...)
          }
)

print.brokenstick <- function ( x, ... ) {
  cat ("knots: ", x@knots,"\n")
  cat ("Boundary.knots: ", x@Boundary.knots,"\n")
  cat ("degree: ", x@degree,"\n")
  print(summary(x))
  invisible(x)
}


# setGeneric("predict")
# setMethod("predict", signature( x = "brokenstick" ),
#           function ( x, ... ) {
#             predict.brokenstick( x, ...)
#           }
# )


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
#' Experience has shown that if 
#' there are enough children relative to the number of specified 
#' random effects, the variance-variance matrix will have elements
#' that diminish as they move away from the diagonal, as one would
#' expect for data without seasonality, like growth data.
#' 
#' This function can be time consuming for data sets with several hundreds
#' of children.
#' @aliases brokenstick
#' @param y a vector containing the measurements to be analyzed
#' @param x a vector of \code{length(y)} with the predictor on which 
#' the break points should be defined. In longitudinal data, this is usually decimal age.
#' @param subject a vector containing the subject identification
#' @param knots a numerical vector with the locations of the breaks to be 
#' placed on the values of \code{x}. This setting is passed to \code{bs()}. Specify \code{knots} to include the range of \code{x}. This will evade the warning 
#' \code{some 'x' values beyond boundary knots may cause ill-conditioned bases}, a situation that may lead to nonsensical results. If in doubt, set argument \code{storeX = TRUE} and inspect the \code{X} slot of the result. The \code{X} matrix should have values between 0 and 1, and each row should add up to 1.
#' @param Boundary.knots a numerical vector with the locations of the 
#' outer break point for \code{x}. This setting is passed to \code{bs()}. The default is to set the left boundary knot equal to the \code{min(knots)}. The right boundary knots is taken as the larger of \code{max(knots)} and the maximum of \code{x}.
#' @param degree the degree of the B-spline. For the broken stick model this 
#' should be to 1 (the default), which specifies that values between the break
#' points are located on a straight line.
#' @param control A function to control fitting of \code{lmer()}. The default
#' is set to \code{lmerControl(check.nobs.vs.nRE = "warning")}, which turn
#' fatal errors with respect the number of parameters into warnings.
#' @param storeX A logical indicating whether the spline model matrix should be returned as slot \code{X} in the result. The default is \code{FALSE}
#' @param \dots Additional arguments passed down to \code{lmer()} 
#' (e.g. to specify additional \code{lmer()} options.
#'  @return A fitted model of class \code{brokenstick}, which extends the 
#'  class \code{lmerMod}
#' @examples 
#' library(mice)
#' data <- tbc[tbc$id < 1000 & tbc$age < 2.5,]
#' fit <- brokenstick(y = data$hgt.z, x = data$age, subject = data$id, 
#'                    knots = c(0, 1, 2))
#' plot(fit)
#' @export
brokenstick <- function(y, x, subject, 
                        knots = pretty(x),
                        Boundary.knots = 
                          c(min(knots),
                            max(max(x, na.rm = TRUE, max(knots)))),
                        degree = 1,
                        control = lmerControl(check.nobs.vs.nRE = "warning"), 
                        storeX = FALSE,
                        ...) {
  X <- bs(x = x, knots = knots, Boundary.knots = Boundary.knots, 
          degree = degree)
  colnames(X) <- paste("x", 1:ncol(X), sep = "")
  pred <- paste("0 +", paste(colnames(X), collapse = " + "))
  data <- na.omit(data.frame(subject = subject, x = x, y = y, X))
  f <- as.formula(paste("y", "~", pred, 
                        "+ (", pred, "| subject)"))
  fit <- lmer(f, data = data,
              control = control,
              ...)
  # attr(fit, "model") <- "brokenstick"
  class(fit) <- "brokenstick"
  fit@knots <- knots
  fit@Boundary.knots <- Boundary.knots
  fit@degree <- degree
  if (storeX) fit@X <- X
  return(fit)
}

