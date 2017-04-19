#'Plot observed and fitted trajectories
#'
#'The \code{plot} method for a \code{brokenstick} object plots the observed and
#'fitted trajectories of one or more individuals.
#'
#'@param x An object of class \code{brokenstick} or \code{brokenstick_export}
#' @param py A vector with measurements using the same response scale as the
#'   fitted model. Used as \code{y} in \code{\link{predict.brokenstick}}.
#' @param px A vector with decimal ages of length \code{length(py)}.
#'   fitted model. Used as \code{x} in \code{\link{predict.brokenstick}}.
#'@param ids A vector with one or more subject identification codes.
#'@param x_trim A range to select the x-axis
#'@param max_ids A scalar indicating the maximum number of individual plots. The default is 3, which select the first six figures. Setting \code{max_ids} to a high number increses the maximum. The parameter implements a safety measure to avoid unintended plots of the entire data set.
#'@param show_measurements A logical indicating whether the measurement should be plotted. The default is \code{TRUE}.
#'@param show_estimates A logical indicating whether the estimates should be plotted. The default is \code{TRUE}.
#'@param ... Extra arguments passed down to \code{\link[rbokeh]{figure}} and
#' \code{\link[rbokeh]{ly_lines}}, \code{\link[rbokeh]{ly_points}},
#' '\code{\link[hbgd]{ly_zband}} and '\code{\link[rbokeh]{grid_plot}} functions.
#'@return An object of class \code{rbokeh}.
#'@author Stef van Buuren 2016
#'@method plot brokenstick
#'@examples
#'smc <- smocc_hgtwgt
#'fit <- brokenstick(y=smc$haz, x=smc$age, subj = smc$subjid,
#'  knots = 0:2, boundary = c(0, 3))
#'plot(fit, ids = c(10001, 10004, 10005),
#'  px = get_knots(fit), x_trim = c(0, 2.2), size.y = 6,
#'  size.yhat = 6, height = 300, width = 600)
#'@export
plot.brokenstick <- function(x, py, px, ids = NULL,
                             x_trim = c(-Inf, Inf), max_ids = 3,
                             show_measurements = TRUE,
                             show_estimates = TRUE, ...) {
  if (!inherits(x, "brokenstick")) stop ("Argument `x` not of class brokenstick.")

  # calculate brokenstick predictions, long format
  if (show_estimates & missing(px)) px <- get_knots(x)
  if (missing(py)) pr <- predict(object = x, x = px, ...)
  else pr <- predict(object = x, y = py, x = px, ...)

  # plot all trajectories if ids == NULL
  if (is.null(ids)) idx <- pr$x >= x_trim[1] & pr$x <= x_trim[2]
  else idx <- pr$subjid %in% ids & pr$x >= x_trim[1] & pr$x <= x_trim[2]
  data <- pr[idx, , drop = FALSE]
  data <- split(data, as.factor(as.character(data$subjid)))

  # safety trunctions
  if (length(data) >= max_ids) data <- data[1:max_ids]
  plot_trajectory(x = x, data = data, ...)
}

#' @inheritParams plot.brokenstick
#' @method plot brokenstick_export
#' @export
plot.brokenstick_export <- function(x, py, px, ids = NULL,
                                    x_trim = c(-Inf, Inf), max_ids = 6, ...) {
  if (!inherits(x, "brokenstick_export")) stop ("Argument `x` not of class brokenstick.")

  # calculate brokenstick predictions, long format
  mpy <- missing(py)
  mpx <- missing(px)
  if (mpy & mpy) pr <- predict(object = x, ...)
  else if (mpy) pr <- predict(object = x, x = px, ...)
  else if (mpx) pr <- predict(object = x, y = py, ...)
  else pr <- predict(object = x, y = py, x = px, ...)

  idx <- pr$x >= x_trim[1] & pr$x <= x_trim[2]
  data <- list(pr[idx, , drop = FALSE])
  plot_trajectory(x = x, data = data, ...)
}

#' Plot observed and fitted trajectories from fitted brokenstick model
#'
#'This function is called by \code{plot.brokenstick()} and \code{plot.brokenstick_export()}, and implements the plotting routines using \code{rbokeh}.
#' @param x An object of class \code{brokenstick} or \code{brokenstick_export}
#' @param data A list of data frames produced by \code{predict.brokenstick} and \code{predict.brokenstick_export}
#' @param color.y A character vector with two elements specifying the symbol and line color of the measured data points
#' @param size.y Dot size of measured data points
#' @param color.yhat A character vector with two elements specifying the symbol and line color of the predicted data points
#' @param size.yhat Dot size of predicted data points
#' @param nrow Number of rows in plot
#' @param height Figure height in pixels
#' @param width Figure width in pixels
#' @param x_range a vector specifying the range (min, max) that the superposed growth standard should span on the x-axis
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param show_reference A logical indicating whether the reference should be
#' added to the plot. The default is \code{TRUE}.
#' @param \dots Parameters passed down to \code{\link[rbokeh]{figure}},
#' \code{\link[rbokeh]{ly_lines}}, \code{\link[rbokeh]{ly_points}}
#' and '\code{\link[rbokeh]{grid_plot}} functions.
#' @return An object of class \code{rbokeh}
#' @export
plot_trajectory <- function(x = x, data = data,
                            color.y = c("blue", "darkgreen"),
                            size.y = 10,
                            color.yhat = c("red", "darkgreen"),
                            size.yhat = 10,
                            height = 520, width = 480,
                            nrow = 1,
                            x_range = c(0, 2),
                            xlab = "Age (years)",
                            ylab = "Length (SDS)",
                            show_reference = TRUE,
                            ...) {
  if (!is.list(data)) stop("Argument `data` should be a list")

  figs <- lapply(data, function(x) {
    # browser()
    k <- x$knot
    fig <- figure(xlab = xlab, ylab = ylab,
                  height = height, width = width, ...)
    if (show_reference)
      fig <- hbgd::ly_zband(fig, x = x_range, z = -c(2.5, 2, 1, 0))
    if (any(!k)) {
      fig <- fig %>%
        ly_lines( x = x$x[!k], y = x$y[!k],
                  color = color.y[2]) %>%
        ly_points(x = x$x[!k], x$y[!k],
                  color = color.y[1], size = size.y)
    }

    if (any(k)) {
      fig <- fig %>%
        ly_lines( x = x$x[k], y = x$yhat[k],
                  color = color.yhat[2]) %>%
        ly_points(x = x$x[k], x$yhat[k],
                  color = color.yhat[1], size = size.yhat)
    }
    fig
  })
  if (length(figs) == 1) return(figs[[1]])
  grid_plot(figs, same_axes = TRUE, simplify_axes = TRUE, nrow = nrow,
            height = height, width = width)
}

#'Get the label of a HBGD standard variable
#'
#'This functions consults the \code{hbgd::hbgd_labels} table to find the variable
#'label.
#'
#'@note This function should be placed in the \code{hbgd} package.
#'@param x An character object with the name of one variable
#'@return A character vector, either the label found in the table, or the name of the variable.
#'@export
get_label <- function(x) {
  label <- hbgd::hbgd_labels[[x]]
  if (is.null(label)) return(x)
  return(label)
}
