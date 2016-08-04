# predict.R

#' Predict growth curve according to the broken stick model
#'
#' Calculation predictions (conditional means of the random effects) from the
#' broken stick model. This function takes an object of class
#' \code{brokenstick}, and returns predictions in one of several formats. We may
#' calculate predictions for new persons, i.e., for persons who are not part of
#' the fitted model, by specifying the \code{x} and \code{y} arguments.
#' @aliases predict.brokenstick
#' @param object An object of class \code{brokenstick} or of class
#'   \code{brokenstick.export}.
#' @param y      A vector with measurements using the same response scale as the
#'   fitted model.
#' @param x    A vector with decimal ages of length \code{length(y)}. 
#' If both \code{y} and \code{x} not specified,
#'   the function calculates the predicted values for all persons on which
#'   \code{object} was fitted. If \code{y} is not specified, but \code{x} is,
#'   then broken stick estimates are obtained at \code{x} for all persons in
#'   \code{object}. Note that the \code{at = "x"} argument is needed to make
#'   predictions at the \code{x} values.
#' @param at   If \code{at = "x"} (the default) the function returns
#'   predictions at the \code{x} values in the data. Specify \code{at =
#'   "knots"} to obtain predictions at the knots of the model.
#' @param output A string specifying the desired type of output. If \code{output
#'   = "long"} (the default), the result is cast into a data frame of the long
#'   format that represents each row represented as an observation. If
#'   \code{output = "broad"}, the result is formed into a broad matrix where
#'   each row represents a person. This format is useful as input to a secondary
#'   data analysis that analyzes the smoothed data as repeated measured. The
#'   format can be used if \code{at = "knots"}, and returns \code{NULL}
#'   otherwise. If \code{output = "vector"}, the result is returned as a vector.
#'   This is the fastest method, and should be used in programming
#'   and simulation.
#' @param include.boundaries Logical indicating whether the broken stick
#'   estimates on the right-hand side boundary should be included. The default
#'   is \code{TRUE}.
#' @param filter_na Logical indicating whether to return estimates for new values
#'   of \code{y} only. The default is \code{FALSE}, so all predictions are
#'   returned. Prediction at a given \code{x} values may be forced by setting
#'   the corresponding entries of \code{y} equal to \code{NA}. Prediction at the
#'   knots is also considered new, even if there are observed values at \code{x}
#'   and \code{y}.
#' @param \dots Additional arguments passed down to \code{predict.merMod()},
#'   \code{predict.brokenstick.export()} and \code{predict.atx()}. Set the flag
#'   \code{filter_na = FALSE} to obtain predictions for both old and new \code{x}.
#' @return If \code{output == "long"}, a data frame with five columns named:
#'   \describe{
#'   \item{\code{id}}{Person identification}
#'   \item{\code{x}}{The \code{x} values of the model, usually age}
#'   \item{\code{y}}{Response values (in scale defined by the model)}
#'   \item{\code{yhat}}{Predicted values}
#'   \item{\code{knot}}{Logical, TRUE indicates that the prediction is
#'   done at the location of the knot, or at a location specified by an 
#'   isolated \code{x} argument, i.e., without a corresponding \code{y} argument.}
#'   }
#'   If \code{output == "broad"}, a
#'   numeric matrix of predicted values with \code{length(slot(object,
#'   "knots"))} columns. If \code{output == "vector"}, a numeric vector of
#'   predicted values corresponding to the column \code{yhat}.
#' @author Stef van Buuren 2016
#' @note The original data are only present in the \code{object} of class
#'   \code{brokenstick}. Hence, any calculations involving observations of the
#'   fitted model require an object of class \code{brokenstick}. Predictions for
#'   new units can be done both with class \code{brokenstick} and
#'   \code{brokenstick.export}.
#' @family brokenstick
#' @examples
#' # Estimates at each measured age for all persons in fitted model
#' class(fit.hgt)
#' p <- predict(fit.hgt)
#' head(p)
#'
#' # Estimates at knots, stored into the broad matrix
#' p <- predict(fit.hgt, output = "broad", at = "knots")
#' round(head(p), 2)
#'
#' # Get these estimates as a vector, useful for programming
#' p <- predict(fit.hgt, output = "vector", at = "knots")
#' round(head(p), 2)
#'
#' # Obtain broken stick estimates at the measured ages
#' p <- predict(fit.hgt)
#' head(p)
#'
#' # Obtain estimates at weeks 1-4 for all children, include old points
#' p <- predict(fit.hgt, x = round((1:4)*7/365.25, 4))
#' head(p)
#'
#' # Same, but now organised as broad matrix of new points only
#' p <- predict(fit.hgt, x = round((1:4)*7/365.25, 4), output = "broad",
#' filter_na = TRUE)
#' head(p)
#'
#' @export
predict.brokenstick <- function(object, y, x, at = "x",
                                output = "long", include.boundaries = TRUE,
                                filter_na = FALSE,
                                ...) {
  at <- match.arg(at, c("knots", "x"))
  output <- match.arg(output, c("vector", "long", "broad"))
  
  # If user did not specify y and x, do everybody in the object
  everybody <- missing(y) && missing(x)
  
  # For everybody, prediction at the measured x
  if (at == "x" &&  everybody) {
    yhat <- fitted(object, ...)
    result <- switch(output,
                     vector = yhat,
                     long = yhat2long(object, yhat,
                                      at = at,
                                      include.boundaries = include.boundaries),
                     broad = NULL)  # not possible
    return(result)
  }
  
  # For everybody, prediction at the knots
  if (at == "knots" && everybody) {
    yhat <- t(lme4::ranef(object)$subject) + lme4::fixef(object)
    rownames(yhat) <- get.knots(object)
    if (!include.boundaries) yhat <- yhat[-nrow(yhat), ]
    result <- switch(output,
                     vector = as.vector(yhat),
                     long = yhat2long(object, yhat,
                                      include.boundaries = include.boundaries,
                                      at = "knots"),
                     broad = t(yhat))
    return(result)
  }
  
  # For everybody, prediction at new break ages (but do NOT change the model)
  # If x is specified, but y not, then predict as `x` for everybody
  newbreak  <- missing(y) && !missing(x)
  if (newbreak) {
    if (length(x) == 0) return(numeric(0))
    return(predict.atx(object, x, output = output,
                       filter_na = filter_na, ...))
  }
  
  # handle predictions for individuals
  # by calling predict.brokenstick.export()
  
  if (length(y) == 0) return(numeric(0))
  if (length(y) != length(x)) stop("Incompatible length of `y` and `x`.")
  
  export <- export.brokenstick(object)
  predict(export, y, x, at = at,
          output = output,
          include.boundaries = include.boundaries,
          filter_na = filter_na, ...)
}



#' Predict growth curve according to the broken stick model
#' @inheritParams predict.brokenstick
#' @aliases predict.brokenstick.export
#' @family brokenstick
#' @examples
#' exp <- export.brokenstick(fit.hgt)
#'
#' # no data predicts mean trajectory
#' p <- predict(exp)
#' head(p)
#'
#' # predict mean trajectory at weeks 1-4
#' predict(exp, x = round((1:4)*7/365.25, 4))
#'
#' # add data at each week
#' predict(exp, x = round((1:4)*7/365.25, 4), y = c(1, 0.8, 0.9, 0.7))
#'
#' # no data at weeks 2 and 3
#' predict(exp, x = round((1:4)*7/365.25, 4), y = c(1, NA, NA, 0.7))
#'
#' # estimates at standard knots
#' predict(exp, x = round((1:4)*7/365.25, 4), y = c(1, NA, NA, 0.7),
#'   at = "knots")
#'
#' # leaving out the missing data produces the same result
#'predict(exp, x = round(c(1,4)*7/365.25, 4), y = c(1, 0.7),
#'   at = "knots")
#' @export
predict.brokenstick.export <- function(object, y, x,
                                       at = "x",
                                       output = "long",
                                       include.boundaries = TRUE,
                                       filter_na = FALSE,
                                       ...) {
  at <- match.arg(at, c("knots", "x"))
  output <- match.arg(output, c("vector", "long", "broad"))
  
  # case: if no `x` is given, just use the knots
  if (missing(x)) {
    x <- get.knots(object, include.boundaries)
    implicit.knots <- TRUE
  } else {
    implicit.knots <- FALSE
  }
  
  # case: if no 'y' is given
  if (missing(y)) y <- rep(NA, length(x))
  if (length(y) == 0 | length(x) == 0) return(numeric(0))
  if (length(y) != length(x)) stop("Incompatible length of `y` and `x`.")
  
  # code the x at which the child is observed as
  # linear splines with given knots
  X <- bs_robust(x = x, knots = object$knots,
                 Boundary.knots = object$Boundary.knots,
                 degree = object$degree)
  colnames(X) <- paste("x", 1:ncol(X), sep = "")
  
  # calculate random effect through empirical Bayes (BLUP) predictor
  bs.z <- EB(object, y = y, X, BS = TRUE)
  
  # knots to use for prediction
  knots <- get.knots(object, include.boundaries)
  if (!include.boundaries) bs.z <- bs.z[-length(bs.z)]
  
  # prediction at knots
  if (at == "knots")
    return(switch(output,
                  vector = bs.z,
                  long = data.frame(id = NA, x = knots, y = NA, yhat = bs.z,
                                    knot = TRUE,
                                    row.names = as.character(1:length(knots)))))
  
  # individual (response) prediction at x
  if (object$degree > 1) stop("Cannot predict for degree > 1")
  yhat <- approx(x = knots, y = bs.z, xout = x)$y
  
  data <- data.frame(id = NA, x = x, y = y, yhat = yhat,
                     knot = implicit.knots,
                     row.names = as.character(1:length(x)))
  if (filter_na) data <- data[is.na(y), ]
  
  # convert to proper output format
  result <- switch(output,
                   vector = data$yhat,
                   long = data)
  return(result)
}


bs_robust <- function(x, df = NULL, knots = NULL, degree = 3, 
                      intercept = FALSE, Boundary.knots = range(x)) {
  # this is a wrapper to splines::bs that 
  # 1) accepts an X that is fully NA
  # 2) suppresses warnings about out-of-range values
  padx <- all(is.na(x)) 
  if (padx) x <- c(0, x)
  X <- suppressWarnings(
    splines::bs(x = x, df = df, knots = knots, degree = degree,
                intercept = intercept, Boundary.knots = Boundary.knots))
  if (padx) X <- X[-1, , drop = FALSE]
  return(X)
}

yhat2long <- function(object, yhat, at = "x",
                      include.boundaries = TRUE) {
  
  if (at == "x") {
    # we need to recalculate x from model.matrix since the lmerMod object
    # does not seem to store the original data
    brk <- get.knots(object)
    y <- model.frame(object)$y
    result <- data.frame(id = model.frame(object)$subject,
                         x = model.matrix(object) %*% brk,
                         y = y,
                         yhat = as.vector(yhat),
                         knot = FALSE,
                         row.names = as.character(1:length(y)))
    return(result)
  }
  
  if (at == "knots") {
    brk <- get.knots(object, include.boundaries)
    grd <- expand.grid(x = brk,
                       id = as.factor(rownames(lme4::ranef(object)$subject)))
    result <- data.frame(id = grd$id,
                         x = grd$x,
                         y = NA,
                         yhat = as.vector(yhat),
                         knot = TRUE,
                         row.names = as.character(1:length(grd$x)))
    return(result)
  }
}




predict.atx <- function(object, x,
                        output = "long",
                        filter_na = FALSE, ...) {
  # auxiliary function to calculate predictions at a common set
  # of x values for all individuals
  # called by predict.brokenstick()
  if (length(x) == 0) return(numeric(0))
  
  export <- export.brokenstick(object)
  
  # recreate the original data
  brk <- get.knots(object)
  data1 <- data.frame(id = model.frame(object)$subject,
                      x = model.matrix(object) %*% brk,
                      y = model.frame(object)$y,
                      knot = FALSE,
                      row.names = as.character(1:nrow(model.matrix(object))))
  
  # construct supplemental data
  grd <- expand.grid(x = x, # x: new break ages
                     id = as.factor(rownames(lme4::ranef(object)$subject)))
  data2 <- data.frame(id = grd$id,
                      x = grd$x,
                      y = NA,
                      knot = TRUE,
                      row.names = as.character(1:length(grd$x)))
  
  # concatenate, sort and split over ID
  data <- rbind(data1, data2)
  data <- data[order(data$id, data$x), ]
  ds <- split(data, f = data$id)
  
  # simple loop over id
  result <- vector("list", length(ds))
  for (i in seq_along(ds)) {
    d <- ds[[i]]
    if (nrow(d) > 0) result[[i]] <- predict(export, y = d$y,
                                            x = d$x, at = "x",
                                            output = "vector",
                                            ...)
  }
  
  # save
  data$yhat <- unlist(result)
  data <- data[, c("id", "x", "y", "yhat", "knot")]
  if (filter_na || output == "broad") data <- data[is.na(data$y), ]
  
  # convert to proper output format
  result <- switch(output,
                   vector = data$yhat,
                   long = data,
                   broad = matrix(data$yhat, ncol = length(x), byrow = TRUE,
                                  dimnames = list(NULL, x)))
  return(result)
}

#' Obtain the knots from a broken stick model
#'
#' @param object An object of class \code{brokenstick} or \code{brokenstick.export}
#' @param include.boundaries A logical specifying whether the right-most (boundary) knots should be included. The default is \code{FALSE}, which return only the internal knots.
#' @return A vector with knot locations
#' @examples
#' get.knots(fit.hgt)
#' @export
get.knots <- function(object, include.boundaries = TRUE) {
  if (inherits(object, "brokenstick")) {
    knots <- c(object@knots, object@Boundary.knots[2])
    if (!include.boundaries) knots <- knots[-length(knots)]
    return(knots)
  }
  
  if (inherits(object, "brokenstick.export")) {
    knots <- c(object$knots, object$Boundary.knots[2])
    if (!include.boundaries) knots <- knots[-length(knots)]
    return(knots)
  }
  return(NULL)
}
