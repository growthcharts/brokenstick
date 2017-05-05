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
#'  px = get_knots(fit), x_trim = c(0, 2.2), xlim = c(0, 2.2))
#'@export
plot.brokenstick <- function(x, py, px, ids = NULL,
                             max_ids = 3,
                             x_trim = c(-Inf, Inf),
                             show_measurements = TRUE,
                             show_estimates = TRUE, ...) {
  if (!inherits(x, "brokenstick")) stop ("Argument `x` not of class brokenstick.")

  # calculate brokenstick predictions, long format
  if (show_estimates & missing(px)) px <- get_knots(x)
  if (missing(py)) pr <- predict(object = x, x = px, ...)
  else pr <- predict(object = x, y = py, x = px, ...)

  # plot first max_ids trajectories if ids == NULL
  if (is.null(ids)) idx <- pr$subjid %in% unique(pr$subjid)[1:max_ids]
  else idx <- pr$subjid %in% ids
  idx <- idx & pr$x >= x_trim[1] & pr$x <= x_trim[2]
  data <- pr[idx, , drop = FALSE]

  plot_trajectory(x = x, data = data, ...)
}

#' @inheritParams plot.brokenstick
#' @method plot brokenstick_export
#' @export
plot.brokenstick_export <- function(x, py, px, ids = NULL,
                                    x_trim = c(-Inf, Inf), max_ids = 3, ...) {
  if (!inherits(x, "brokenstick_export")) stop ("Argument `x` not of class brokenstick.")

  # calculate brokenstick predictions, long format
  mpy <- missing(py)
  mpx <- missing(px)
  if (mpy & mpy) pr <- predict(object = x, ...)
  else if (mpy) pr <- predict(object = x, x = px, ...)
  else if (mpx) pr <- predict(object = x, y = py, ...)
  else pr <- predict(object = x, y = py, x = px, ...)

  idx <- pr$x >= x_trim[1] & pr$x <= x_trim[2]
  data <- pr[idx, , drop = FALSE]
  plot_trajectory(x = x, data = data, ...)
}

#' Plot observed and fitted trajectories from fitted brokenstick model
#'
#' This function plot the observed and fitted trajectories from brokenstick model.
#' Actual plotting may be done by \code{gglot} or \code{rbokeh}.
#' @param x An object of class \code{brokenstick} or \code{brokenstick_export}
#' @param data A list of data frames produced by \code{predict.brokenstick} and \code{predict.brokenstick_export}
#' @param color.y A character vector with two elements specifying the symbol and line color of the measured data points
#' @param size.y Dot size of measured data points
#' @param color.yhat A character vector with two elements specifying the symbol and line color of the predicted data points
#' @param size.yhat Dot size of predicted data points
#' @param ncol Number of columns in plot
#' @param height Figure height in pixels (only bokeh)
#' @param width Figure width in pixels (only bokeh)
#' @param x_range a vector specifying the range (min, max) that the superposed growth standard should span on the x-axis
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param xlim Vector of length 2 with range of x-axis
#' @param ylim Vector of length 2 with range of y-axis
#' @param theme Plotting theme (only ggplot)
#' @param show_reference A logical indicating whether the reference should be
#' added to the plot. The default is \code{FALSE}.
#' @param pkg A string indicating whether the \code{"ggplot"} or
#' \code{"bokeh"} plotting package should be used. The default is \code{"ggplot"}.
#' @param \dots Parameters passed down to \code{\link[rbokeh]{figure}},
#' \code{\link[rbokeh]{ly_lines}}, \code{\link[rbokeh]{ly_points}}
#' and '\code{\link[rbokeh]{grid_plot}} functions.
#' @return An object of class \code{ggplot} or \code{rbokeh}.
#' @rdname plot_trajectory
#' @examples
#' smc <- brokenstick::smocc_hgtwgt
#' knots <- 0:2
#' fit <- brokenstick(y = smc$haz, x = smc$age, subjid = smc$subjid, knots = knots)
#'
#' # plot first three cases
#' plot(fit)
#' @export
plot_trajectory <- function(x, data, pkg = c("ggplot", "bokeh"), ...) {
  pkg <- match.arg(pkg)
  if (pkg == "ggplot") return(plot_trajectory_ggplot(x = x, data = data, ...))
  return(plot_trajectory_bokeh(x = x, data = data, ...))
}

#' @rdname plot_trajectory
plot_trajectory_bokeh <- function(x, data,
                                  color.y = c("blue", "grey"),
                                  size.y = 6,
                                  color.yhat = c("red", "grey"),
                                  size.yhat = 6,
                                  height = 300, width = 680,
                                  ncol = 3,
                                  x_range = c(0, 2),
                                  xlab = "Age (years)",
                                  ylab = "Length (SDS)",
                                  show_reference = FALSE,
                                  xlim = NULL,
                                  ylim = NULL,
                                  ...) {
  # split since rbokeh does not support faceting
  data <- split(data, as.factor(as.character(data$subjid)))

  figs <- lapply(data, function(x) {
    # browser()
    k <- x$knot
    fig <- figure(xlab = xlab, ylab = ylab,
                  height = height, width = width, ...)
    if (!is.null(xlim)) fig <- fig + xlim(xlim)
    if (!is.null(ylim)) fig <- fig + ylim(ylim)
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

  # put together different plots
  if (length(figs) == 1) return(figs[[1]])
  grid_plot(figs, same_axes = TRUE, simplify_axes = TRUE, ncol = ncol,
            height = height, width = width)
}

#' @rdname plot_trajectory
plot_trajectory_ggplot <- function(x, data,
                                   color.y = c("blue", "grey"),
                                   size.y = 2,
                                   color.yhat = c("red", "grey"),
                                   size.yhat = 2,
                                   ncol = 3,
                                   x_range = c(0, 2),
                                   xlab = "Age (years)",
                                   ylab = "Length (SDS)",
                                   show_reference = FALSE,
                                   xlim = NULL,
                                   ylim = NULL,
                                   theme = ggplot2::theme_light(),
                                   ...) {

  g <- ggplot(data, aes_string(x = "x", y = "y")) +
    xlab(xlab) + ylab(ylab)

  if (!is.null(xlim)) g <- g + xlim(xlim)
  if (!is.null(ylim)) g <- g + ylim(ylim)

  if (show_reference)
    g <- hbgd::geom_zband(g, x = x_range,
                          z = -c(2.5, 2, 1, 0),
                          color = "#59a14f")

  # Note color #59a14f is Tableau10 green

  # add observed data points and lines
  k <- data$knot
  if (any(!k)) {
    g <- g +
    geom_line(data = data[!k, ], color = color.y[2]) +
    geom_point(data = data[!k, ], color = color.y[1], size = size.y)
  }

  # add broken stick points and lines
  if (any(k)) {
    g <- g +
      geom_line(aes_string(y = "yhat"),
                data = data[k, ], color = color.yhat[2]) +
      geom_point(aes_string(y = "yhat"),
                 data = data[k, ], color = color.yhat[1],
                 size = size.yhat)
  }

  # split out according to subjid
  g <- g + facet_wrap(~subjid, ncol = ncol)

  # set theme
  g <- g + theme

  return(g)
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
