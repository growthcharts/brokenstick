# predict.R

#' Predict all growth curves by broken stick model
#'
#' Calculation predictions (conditional means of the random effects) from the broken stick model. This function takes an object of class \code{brokenstick}, and returns predictions in one of several formats. One may calculate predictions for new persons, i.e., for persons who are not part of the fitted model, by specifying the \code{age} and \code{y} arguments.
#' @aliases predict.brokenstick
#' @param object An object of class \code{brokenstick}.
#' @param y      A vector with measurements using the same response scale as the fitted model.
#' @param age    A vector with decimal ages of length \code{length(y)}. Age must be sorted in increasing order, and within the age range defined by \code{range(object@knots)}. If both \code{y} and \code{age} not specified, the function calculates the predicted values for all persons on which \code{object} was fitted. If \code{y} is not specified, but \code{age} is, then \code{age} broken stick estimates are obtained at these ages for all persons in \code{object}.
#' @param type   If \code{type = "atknots"} (the default), the function returns the conditional means at the break ages. If \code{type = "atage"} the function returns the conditional means at the measurement ages. This argument is relevant only if both \code{y} and \code{age} are left unspecified.
#' @param output A string specifying the desired type of output. If  \code{output = "long"} (the default), the result is cast into a data frame of the long format that represents each row represented as an observation. If \code{output = "broad"}, the result is formed into a broad matrix where each row represents a person. This format is useful as input to a secondary data analysis that analyzes the smoothed data as repeated measured. The format can be used if \code{type = "atknots"}, and returns \code{NULL} otherwise. If \code{output = "vector"}, the result is returned as a vector. This is the fastest method, and should preferably be used in programming and simulation.
#' @param include.boundaries Logical indicating whether the broken stick estimates on the right-hand side boundary should be included. The default is \code{FALSE}.
#' @param \dots Additional arguments passed down to \code{fitted()}, \code{predict.brokenstick.export()} and \code{predict.atage()}. Set the flag \code{onlynew = FALSE} to obtain predictions for both old and new ages.
#' @return If \code{output == "long"}, a data frame with four columns named \code{id} (the person identification), \code{age}, \code{y} (the response variable, set to \code{NA} for \code{type == "atknots"}) and \code{yhat} (predicted values). If \code{output == "broad"}, a numeric matrix of predicted values with \code{length(slot(object, "knots"))} columns. If \code{output == "vector"}, a numeric vector of predicted values.
#' @author Stef van Buuren 2016
#' @examples
#' # Obtain broken stick estimates at break ages for all persons in fitted model
#' p <- predict(fit.hgt)
#' head(p)
#' 
#' # Same, but now stored into the broad matrix
#' p <- predict(fit.hgt, output = "broad")
#' round(head(p), 2)
#' 
#' # Get estimates as a vector, useful for programming
#' p <- predict(fit.hgt, output = "vector")
#' round(head(p), 2)
#' 
#' # Obtain broken stick estimates at the measured ages
#' p <- predict(fit.hgt, type = "atage")
#' head(p)
#' 
#' # Obtain estimates at weeks 1-4 for all children, include old points
#' p <- predict(fit.hgt, age = round((1:4)*7/365.25, 4), onlynew = FALSE)
#' head(p)
#' 
#' # Same, but as broad matrix of new points
#' p <- predict(fit.hgt, age = round((1:4)*7/365.25, 4), output = "broad")
#' head(p)
#' 
#' @export
predict.brokenstick <- function(object, y, age, type = "atknots", 
                                output = "long", include.boundaries = FALSE, 
                                ...) {
  type <- match.arg(type, c("atknots", "atage"))
  output <- match.arg(output, c("vector", "long", "broad"))
  
  # If user did not specify y and age, do everybody in the object
  everybody <- missing(y) && missing(age)
  
  # For everybody, prediction at the measurement ages
  if (type == "atage" &&  everybody) {
    yhat <- fitted(object, ...)
    result <- switch(output,
                     vector = yhat,
                     long = yhat2long(object, yhat, 
                                      type = type),
                     broad = NULL)  # not possible
    return(result)
  }
  
  # For everybody, prediction at the broken stick ages
  if (type == "atknots" && everybody) {
    yhat <- t(lme4::ranef(object)$subject) + lme4::fixef(object)
    rownames(yhat) <- c(object@knots, object@Boundary.knots[2])
    if (!include.boundaries) yhat <- yhat[-nrow(yhat), ]
    result <- switch(output,
                     vector = as.vector(yhat),
                     long = yhat2long(object, yhat, 
                                      include.boundaries = include.boundaries),
                     broad = t(yhat))
    return(result)
  }
  
  # For everybody, prediction at new break ages (but do NOT change the model)
  # If age is specified, but y not, then predict as `age` for everybody
  newbreak  <- missing(y) && !missing(age)
  if (length(age) == 0) return(numeric(0))
  if (newbreak) return(predict.atage(object, age, output = output, ...))
  
  # handle predictions for individuals
  # by calling predict.brokenstick.export()
  
  if (length(y) == 0) return(numeric(0))
  if (length(y) != length(age)) stop("Incompatible length of `y` and `age`.")
  
  export <- export.brokenstick(object)
  predict(export, y, age, type = type, output = output,
          include.boundaries = include.boundaries, ...)
}

#' Predict growth curve according to the broken stick model
#'
#' For a given child, extract the conditional modes of the
#' random effects as specified by the broken stick (=linear
#' piecewise spline) model. 
#' @aliases predict.brokenstick.export
#' @param object   An object of 
#' containing the estimated parameters of the broken stick model
#' @param y     A vector with measurements 
#' @param age   A vector with decimal ages of length \code{length(y)}
#' @param type  If \code{type = "curve"} (the default) 
#' the function returns the broken stick estimates. 
#' If \code{type = "response"}, the function returns a predicted value 
#' for each element of \code{y}.
#' @param \dots Additional arguments (not used)
#' @return A numeric vector of length \code{length(slot(object, "knots"))
#' + slot(object, "degree")} with 
#' predicted values
#' @author Stef van Buuren
#' @export
predict.brokenstick.export <- function(object, y, age, 
                                       type = "atknots", 
                                       output = "long", 
                                       include.boundaries = FALSE, 
                                       onlynew = TRUE,
                                       ...) {
  type <- match.arg(type, c("atknots", "atage"))
  output <- match.arg(output, c("vector", "long", "broad"))
  
  # if the argument `y` or `age` is not given, 
  #if (missing(age)) return(NULL)
  #if (missing(y)) y <- rep(NA, length(age))
  if (missing(age) || missing(y)) return(NULL)
  
  if (length(y) == 0 | length(age) == 0) return(numeric(0))
  if (length(y) != length(age)) stop("Incompatible length of `y` and `age`.")
  
  # code the ages at which the child is observed as
  # linear splines with given break ages
  X <- bs(x = age, knots = object$knots, 
          Boundary.knots = object$Boundary.knots, 
          degree = object$degree)
  colnames(X) <- paste("x", 1:ncol(X), sep = "")
  
  # calculate random effect through empirical Bayes (BLUP) predictor
  bs.z <- EB(object, y = y, X, BS = TRUE)
  if (type == "atknots") yhat <- bs.z
  else { 
    # individual (response) prediction
    if (object$degree > 1) stop("Cannot predict for degree > 1")
    brk <- c(object$knots, object$Boundary.knots[2])
    yhat <- approx(x = brk, y = bs.z, xout = age)$y
  }

  # create output arrays
  data <- data.frame(id = NA, age = age, y = y, yhat = yhat, new = is.na(y))
  if (onlynew) data <- data[data$new, ]
  
  # convert to proper output format
  result <- switch(output,
                   vector = data$yhat,
                   long = data,
                   broad = NULL)
  return(result)
}


yhat2long <- function(object, yhat, type = "atknots", 
                      include.boundaries = FALSE) {
  
  if (type == "atage") {
    # we need to recalculate age from model.matrix since the lmerMod object
    # does not seem to store the original data
    brk <- c(object@knots, object@Boundary.knots[2])
    result <- data.frame(id = model.frame(object)$subject,
                         age = model.matrix(object) %*% brk,
                         y = model.frame(object)$y,
                         yhat = as.vector(yhat))
    return(result)
  }
  
  if (type == "atknots") {
    brk <- c(object@knots, object@Boundary.knots[2])
    if (!include.boundaries) brk <- brk[-length(brk)]
    grd <- expand.grid(age = brk, 
                       id = as.factor(rownames(lme4::ranef(object)$subject)))
    result <- data.frame(id = grd$id, 
                         age = grd$age,
                         y = NA,
                         yhat = as.vector(yhat))
    return(result)
  }
}




predict.atage <- function(object, age, 
                          output = "long", 
                          onlynew = TRUE, ...) {
  # auxiliary function to calculate predictions at a common set of ages
  # for all individuals
  # called by predict.brokenstick()
  if (length(age) == 0) return(numeric(0))
  
  export <- export.brokenstick(object)
  
  # extract the original data
  brk <- c(object@knots, object@Boundary.knots[2])
  data1 <- data.frame(id = model.frame(object)$subject,
                      age = model.matrix(object) %*% brk,
                      y = model.frame(object)$y,
                      new = FALSE)
  
  # construct supplemental data
  grd <- expand.grid(age = age, # age: new break ages
                     id = as.factor(rownames(lme4::ranef(object)$subject)))
  data2 <- data.frame(id = grd$id, 
                      age = grd$age,
                      y = NA,
                      new = TRUE)
  
  # concatenate, sort and split over ID
  data <- rbind(data1, data2)
  data <- data[order(data$id, data$age), ]
  ds <- split(data, f = data$id)
  
  # simple loop over id
  result <- vector("list", length(ds))
  for (i in seq_along(ds)) {
    d <- ds[[i]]
    if (nrow(d) > 0) result[[i]] <- predict(export, y = d$y, 
                                            age = d$age, type = "atage",
                                            output = "vector", 
                                            onlynew = FALSE)
  }
  
  # save
  data$yhat <- unlist(result)
  data <- data[, c("id", "age", "y", "yhat", "new")]
  if (onlynew || output == "broad") data <- data[data$new, ]
  
  # convert to proper output format
  result <- switch(output,
                   vector = data$yhat,
                   long = data,
                   broad = matrix(data$yhat, ncol = length(age), byrow = TRUE, 
                                  dimnames = list(NULL, age)))
  return(result)
}
