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
#'   \code{brokenstick_export}.
#' @param y      A vector with measurements using the same response scale as the
#'   fitted model.
#' @param x    A vector with decimal ages of length \code{length(y)}.
#' If both \code{y} and \code{x} not specified,
#'   the function calculates the predicted values for all persons on which
#'   \code{object} was fitted. If \code{y} is not specified, but \code{x} is,
#'   then broken stick estimates are obtained at \code{x} for all persons in
#'   \code{object}. Note that the \code{at = "x"} argument is needed to make
#'   predictions at the \code{x} values.
#' @param ids A vector with one or more subject identification codes.
#' @param at   If \code{at = "x"} (the default) the function returns
#'   predictions at the \code{x} values in the data. Specify \code{at =
#'   "knots"} to obtain predictions at the knots of the model, or
#'   \code{at = "both"} to obtain both.
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
#' @param \dots Additional arguments passed down to \code{predict.merMod()},
#'   \code{predict.brokenstick_export()} and \code{predict_all()}.
#' @return If \code{output == "long"}, a data frame with five columns named:
#'   \describe{
#'   \item{\code{subjid}}{Person identification}
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
#'   \code{brokenstick_export}.
#' @family brokenstick
#' @examples
#' # Estimates at each measured age for all persons in fitted model
#' class(fit_206)
#' p <- predict(fit_206)
#' head(p)
#'
#' # Estimates at knots, stored into the broad matrix
#' p <- predict(fit_206, output = "broad", at = "knots")
#' round(head(p), 2)
#'
#' # Get these estimates as a vector, useful for programming
#' p <- predict(fit_206, output = "vector", at = "knots")
#' round(head(p), 2)
#'
#' # Obtain estimates at weeks 1-4 for all children, include old points
#' p <- predict(fit_206, x = round((1:4)*7/365.25, 4))
#' head(p)
#'
#' # Same, but now organised as broad matrix of new points only
#' p <- predict(fit_206, x = round((1:4)*7/365.25, 4), output = "broad")
#' head(p)
#'
#' head(predict(fit_206, at = "knots", output = "broad"), 3)
#' predict(fit_206, id = c(10001, 10003), at = "knots", output = "broad")
#' @export
predict.brokenstick <- function(object, y, x, ids = NULL,
                                at = "x", output = "long",
                                ...) {
  at <- match.arg(at, c("knots", "x", "both"))
  output <- match.arg(output, c("vector", "long", "broad"))

  # If user did not specify y, x and ids, return predictions
  # for everybody in the object
  if (missing(y) && missing(x) && is.null(ids)) {
    return(predict_all(object, at = at, output = output))
  }

  # If user specified x, but no y and ids, calculate prediction
  # at the specified x values for all individuals
  if (missing(y) && !missing(x) && is.null(ids)) {
    if (length(x) == 0) return(numeric(0))
    return(predict_all_atx(object, x = x, output = output))
  }

  # If user specified x and y but no ids, treat x and y values as
  # coming from a single individual (so fit and predict)
  # y values with observed data are fitted, y values with NA are predicted
  if (!missing(y) && !missing(x) && is.null(ids)) {
    if (length(y) == 0 || length(x) == 0) return(numeric(0))
    if (length(y) != length(x)) stop("Incompatible length of `y` and `x`.")
    exp <- export(object)
    return(predict(exp, y, x, at = at, output = output))
  }

  # If user specified ids, but no x or y, return model predictions for
  # those ids at the measured and/or knot locations
  if (missing(x) && missing(y) && !is.null(ids)) {
    return(predict_ids_atx(object, ids = ids, at = at, output = output))
  }

  # If user specified ids and x, but no y, return model prediction for
  # those ids at the x locations
  if (!missing(x) && missing(y) && !is.null(ids)) {
    return(predict_ids_atx(object, x = x, ids = ids, at = at, output = output))
  }

  # ELSE
  # If user specified ids, x and y, then treat these as length(ids) new
  # persons
  if (length(y) == 0 || length(x) == 0) return(numeric(0))
  if (length(y) != length(x)) stop("Incompatible length of `y` and `x`.")
  if (!missing(x) && !missing(y) && !is.null(ids))
    exp <- export(object)
  return(predict(exp, y, x, ids = ids, at = at, output = output))
}


#' Predict growth curve according to the broken stick model
#' @inheritParams predict.brokenstick
#' @param subjid A subject identification
#' @aliases predict.brokenstick_export
#' @family brokenstick
#' @examples
#' exp <- export(fit_206)
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
predict.brokenstick_export <- function(object, y, x,
                                       at = "x",
                                       output = "long",
                                       subjid = NA,
                                       ...) {
  at <- match.arg(at, c("knots", "x", "both"))
  output <- match.arg(output, c("vector", "long", "broad"))
  knots <- get_knots(object)

  # case: if no `x` is given, just use the knots
  mx <- missing(x)
  if (mx) {
    x <- get_knots(object)
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
  X <- make_basis(x = x, knots = object$knots,
                 boundary = object$boundary,
                 degree = object$degree, warn = FALSE)
  colnames(X) <- paste("x", 1:ncol(X), sep = "")

  # calculate random effect through empirical Bayes (BLUP) predictor
  bs.z <- EB(object, y = y, X)

  # collect all estimates
  # predict at model knots
  long_knots <- data.frame(subjid = subjid,
                           x = knots,
                           y = NA,
                           yhat = bs.z,
                           knot = TRUE,
                           row.names = as.character(1:length(knots)))
  if (at == "knots") return(
    switch(output,
           vector = as.vector(bs.z),
           long = long_knots,
           broad = matrix(bs.z, ncol = length(knots), byrow = TRUE,
                          dimnames = list(as.character(subjid),
                                          as.character(knots)))))

  # predict at user-specified x values
  if (object$degree > 1) stop("Cannot predict for degree > 1")
  yhat <- approx(x = knots, y = bs.z, xout = x)$y
  long_x <- data.frame(subjid = subjid,
                       x = x,
                       y = y,
                       yhat = yhat,
                       knot = implicit.knots,
                       row.names = as.character(1:length(x)))
  # long_x <- long_x[order(long_x$x), ]
  if (at == "x") return(
    switch(output,
           vector = long_x$yhat,
           long = long_x,
           broad = NULL))

  # combine prediction at knots and x values
  long_both <- rbind(long_x, long_knots)
  # long_both <- long_both[order(long_both$x), ]
  if (at == "both") return(
    switch(output,
           vector = long_both$yhat,
           long = long_both,
           broad = NULL))

  return(NULL)
}

predict_all <- function(object, at, output) {
  if (!inherits(object, "brokenstick"))
    stop("object not of class brokenstick")

  # For everybody, prediction at the measured x
  if (at == "x") {
    r <- switch(output,
                vector = fitted(object),
                long = yhat2long(object, fitted(object), at = at),
                broad = NULL)
    return(r)
  }

  # For everybody, prediction at the knots
  if (at == "knots") {
    yhat <- t(lme4::ranef(object)$subjid) + lme4::fixef(object)
    rownames(yhat) <- get_knots(object)
    r <- switch(output,
                vector = as.vector(yhat),
                long = yhat2long(object, yhat, at = at),
                broad = t(yhat))
    return(r)
  }

  # For everybody, prediction at the measured x and knots
  if (at == "both") {
    long <- yhat2long(object, at = at)
    r <- switch(output,
                vector = long$yhat,
                long = long,
                broad = NULL)
    return(r)
  }
  return(NULL)
}


predict_all_atx <- function(object, x,
                            output = "long",
                            filter_na = FALSE, ...) {
  # auxiliary function to calculate predictions at a common set
  # of x values for all individuals
  # called by predict.brokenstick()
  if (length(x) == 0) return(numeric(0))

  export <- export(object)

  # recreate the original data
  brk <- get_knots(object)
  data1 <- data.frame(get_xy(object),
                      knot = FALSE)

  # construct supplemental data
  grd <- expand.grid(x = x, # x: new break ages
                     subjid = as.factor(rownames(lme4::ranef(object)$subjid)))
  data2 <- data.frame(subjid = grd$subjid,
                      x = grd$x,
                      y = NA,
                      knot = TRUE,
                      row.names = as.character(1:length(grd$x)))

  # concatenate, sort and split over subjid
  data <- rbind(data1, data2)
  data <- data[order(data$subjid, data$knot), ]
  ds <- split(data, f = data$subjid)

  # simple loop over subjid
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
  data <- data[, c("subjid", "x", "y", "yhat", "knot")]
  if (filter_na || output == "broad") data <- data[is.na(data$y), ]

  # convert to proper output format
  result <- switch(output,
                   vector = data$yhat,
                   long = data,
                   broad = matrix(data$yhat, ncol = length(x), byrow = TRUE,
                                  dimnames = list(NULL, x)))
  return(result)
}


yhat2long <- function(object, yhat = NULL, at = "x") {

  brk <- get_knots(object)
  if (at == "x") {
    data <- get_xy(object)
    result <- data.frame(data,
                         yhat = as.vector(yhat),
                         knot = FALSE)
    return(result)
  }

  if (at == "knots") {
    grd <- expand.grid(x = brk,
                       subjid = as.factor(rownames(lme4::ranef(object)$subjid)))
    result <- data.frame(subjid = grd$subjid,
                         x = grd$x,
                         y = NA,
                         yhat = as.vector(yhat),
                         knot = TRUE,
                         row.names = as.character(1:length(grd$x)))
    return(result)
  }

  if (at == "both") {
    data <- get_xy(object)
    yhat1 <- fitted(object)
    yhat2 <- t(lme4::ranef(object)$subjid) + lme4::fixef(object)
    data1 <- data.frame(data,
                        yhat = as.vector(yhat1),
                        knot = FALSE)
    grd <- expand.grid(x = brk,
                       subjid = as.factor(rownames(lme4::ranef(object)$subjid)))
    data2 <- data.frame(subjid = grd$subjid,
                        x = grd$x,
                        y = NA,
                        yhat = as.vector(yhat2),
                        knot = TRUE,
                        row.names = as.character(1:length(grd$x)))

    # concatenate, sort and split over subjid
    data <- rbind(data1, data2)
    data <- data[order(data$subjid, data$knot), ]
    return(data)
  }
  return(NULL)
}


####
predict_atx_experimental <- function(object, x, ids = NULL,
                                     output = "long", ...) {
  # auxiliary function to calculate predictions at a common set
  # of x values for individuals in ids
  # called by predict.brokenstick()
  k <- length(x)
  if (k == 0) return(numeric(0))

  export <- export(object)

  # recreate the original data for ids subset
  data1 <- data.frame(get_xy(object, ids = ids), knot = FALSE)

  # simple loop over subjid
  data2 <- split(data1, f = data1$subjid)
  result <- vector("list", length(data2))
  for (i in seq_along(result)) {
    y <- c(data2[[i]]$y, rep(NA, k))
    px <- c(data2[[i]]$x, x)
    knot <- c(rep(FALSE, nrow(data2[[i]])), rep(TRUE, k))
    yhat <- predict(export, x = px, y = y, output = "vector")
    data <- data.frame(subjid = names(data2)[i],
                       x = px,
                       y = y,
                       yhat = yhat,
                       knot = knot)
    result[[i]] <- data[order(data$subjid, data$knot), ]
  }
  result2 <- do.call(rbind, result)
  result3 <- switch(output,
                    vector = result2$yhat,
                    long = result2,
                    broad = NULL)
  # broad = matrix(data$yhat, ncol = length(x), byrow = TRUE,
  #                dimnames = list(NULL, x)))
  return(result3)
}


predict_ids <- function(object, ids = NULL, at = "x", output = "long") {
  if (!inherits(object, "brokenstick"))
    stop("object not of class brokenstick")

  if (is.null(ids)) return(predict_all(object, at = at, output = output))

  # Prepare to call predict.brokenstick_export() for selected ids
  exp <- export(object)
  knots <- get_knots(object)
  data <- get_xy(object, ids = ids)
  data <- split(data, data$subjid, drop = TRUE)
  result <- vector("list", length(data))
  subjids <- names(result) <- names(data)

  # FIXME: write lapply version for speed
  for (i in 1:length(data)) {
    local_x <- data[[i]]$x
    local_y <- data[[i]]$y
    if (at == "knots" || at == "both") {
      local_x <- c(local_x, knots)
      local_y <- c(local_y, rep(NA, length(knots)))
    }
    result[[i]] <- predict(exp, x = local_x, y = local_y,
                           subjid = subjids[i], output = "long", at = at)
  }
  if (output == "list") return(result)
  result <- do.call(rbind, result)
  if (at == "x" || at == "both")
    r <- switch(output,
                vector = result$yhat,
                long = result,
                broad = NULL)
  if (at == "knots")
    r <- switch(output,
                vector = result$yhat,
                long = result,
                broad = matrix(result$yhat, ncol = length(knots),
                               byrow = TRUE,
                               dimnames = list(as.character(subjids),
                                               as.character(knots))))
  return(r)
}

predict_ids_atx <- function(object, x, ids = NULL, at = "x", output = "long") {
  if (!inherits(object, "brokenstick"))
    stop("object not of class brokenstick")

  if (is.null(ids)) return(predict_all(object, at = at, output = output))
  if (missing(x)) x <- NULL

  # Prepare to call predict.brokenstick_export() for selected ids
  knots <- get_knots(object)
  exp <- export(object)
  data <- get_xy(object, ids = ids)
  data <- split(data, data$subjid, drop = TRUE)
  result <- vector("list", length(data))
  subjids <- names(result) <- names(data)

  # FIXME: write lapply version for speed
  for (i in 1:length(data)) {
    local_x <- c(data[[i]]$x, x)
    local_y <- c(data[[i]]$y, rep(NA, length(x)))
    result[[i]] <- predict(exp, x = local_x, y = local_y,
                           subjid = subjids[i], output = "long", at = at)
  }
  if (output == "list") return(result)
  result <- do.call(rbind, result)
  if (at == "x" || at == "both")
    r <- switch(output,
                vector = result$yhat,
                long = result,
                broad = NULL)
  if (at == "knots")
    r <- switch(output,
                vector = result$yhat,
                long = result,
                broad = matrix(result$yhat, ncol = length(knots),
                               byrow = TRUE,
                               dimnames = list(as.character(subjids),
                                               as.character(knots))))
  return(r)
}
