#' Plot observed and fitted trajectories
#'
#' The \code{plot} method for a \code{brokenstick} object plots the observed and
#' fitted trajectories of one or more individuals.
#'
#' @param x An object of class \code{brokenstick} or \code{brokenstick_export}
#' @param py A vector with measurements using the same response scale as the
#'   fitted model. Used as \code{y} in \code{\link{predict.brokenstick}}.
#' @param px A vector with decimal ages of length \code{length(py)}.
#'   fitted model. Used as \code{x} in \code{\link{predict.brokenstick}}.
#' @param ids A vector with one or more subject identification codes.
#' @param x_trim A range on the x-axis that can be used to subset values that are
#' displayed
#' @param max_ids A scalar indicating the number of individual plots. The default is 3, which plots the trajectories of the first three persons. The \code{max_ids} is a safety measure to prevent unintended plots of the entire data set.
#' @param measurements A logical indicating whether the measurements should be plotted. The default is \code{TRUE}.
#' @param estimates A logical indicating whether the broken stick estimates should be plotted.
#' By default, it is \code{TRUE} if there are internal knots, and \code{FALSE} if there are
#' only boundary knots.
#' @param ... Extra arguments passed down to \code{\link[rbokeh]{figure}} and
#' \code{\link[rbokeh]{ly_lines}}, \code{\link[rbokeh]{ly_points}},
#' '\code{\link[hbgd]{ly_zband}} and '\code{\link[rbokeh]{grid_plot}} functions.
#' @return An object of class \code{rbokeh}.
#' @author Stef van Buuren 2017
#' @method plot brokenstick
#' @examples
#' library("brokenstick")
#' dat <- smocc_hgtwgt
#' # fit one line model for data exploration
#' fit <- brokenstick(y = dat$htcm, x = dat$age, subj = dat$subjid)
#'
#' # plot measurements for first three cases
#' plot(fit, zband = FALSE, est = FALSE)
#' plot(fit, zband = FALSE, est = FALSE, pkg = "bokeh", width = 250, height = 350)
#'
#' # fit model with knots at 1 and 2 years
#' fit <- brokenstick(y = dat$haz, x = dat$age, subj = dat$subjid, knots = 1:2)
#'
#' # ggplot
#' plot(fit, xlim = c(0, 2.2), ylim = c(-3, 3))
#' plot(fit, ids = c(10005, 10012), xlim = c(0, 2.2))
#'
#' # bokeh plots with using x_trim
#' plot(fit, pkg = "bokeh", width = 250, height = 350, x_trim = c(0, 2.2))
#'
#' # bokeh plots with xlim and ylim
#' plot(fit, xlim = c(0, 2), ylim = c(-3, 3), pkg = "b", width = 250, height = 350)
#' plot(fit, ids = 10005, pkg = "bokeh", width = 350, height = 300)
#' @export
plot.brokenstick <- function(x, py, px, ids = NULL,
                             max_ids = 3,
                             x_trim = c(-Inf, Inf),
                             measurements = TRUE,
                             estimates = NULL, ...) {
  if (!inherits(x, "brokenstick")) stop("Argument `x` not of class brokenstick.")

  # define default behavior of estimates flag
  internal_knots <- get_knots(x, "knots")
  if (is.null(estimates)) {
    estimates <-
      !is.null(internal_knots) & length(internal_knots) > 0
  }

  # calculate brokenstick predictions, long format
  if (estimates & missing(px)) px <- get_knots(x)
  if (missing(py)) {
    pr <- predict(object = x, x = px, ...)
  } else {
    pr <- predict(object = x, y = py, x = px, ...)
  }

  # plot first max_ids trajectories if ids == NULL
  if (is.null(ids)) {
    idx <- pr$subjid %in% unique(pr$subjid)[1:max_ids]
  } else {
    idx <- pr$subjid %in% ids
  }
  idx <- idx & pr$x >= x_trim[1] & pr$x <= x_trim[2]
  data <- pr[idx, , drop = FALSE]

  plot_trajectory(x = x, data = data, ...)
}

#' @inheritParams plot.brokenstick
#' @method plot brokenstick_export
#' @export
plot.brokenstick_export <- function(x, py, px, ids = NULL,
                                    x_trim = c(-Inf, Inf), max_ids = 3, ...) {
  if (!inherits(x, "brokenstick_export")) stop("Argument `x` not of class brokenstick.")

  # calculate brokenstick predictions, long format
  mpy <- missing(py)
  mpx <- missing(px)
  if (mpy & mpy) {
    pr <- predict(object = x, ...)
  } else if (mpy) {
    pr <- predict(object = x, x = px, ...)
  } else if (mpx) {
    pr <- predict(object = x, y = py, ...)
  } else {
    pr <- predict(object = x, y = py, x = px, ...)
  }

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
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param xlim Vector of length 2 with range of x-axis
#' @param ylim Vector of length 2 with range of y-axis
#' @param theme Plotting theme (only ggplot)
#' @param zband A logical indicating whether the Z-score band should be
#' added to the plot. The default is \code{TRUE}.
#' @param zband_range a vector specifying the range (min, max) that the superposed growth standard should span on the x-axis. The
#' default is the entire data range.
#' @param pkg A string indicating whether the \code{"ggplot2"} or
#' \code{"bokeh"} plotting package should be used. The default is \code{"ggplot2"}.
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
plot_trajectory <- function(x, data, pkg = c("ggplot2", "bokeh"), ...) {
  pkg <- match.arg(pkg)
  if (pkg == "ggplot2") {
    return(plot_trajectory_ggplot(x = x, data = data, ...))
  }
  return(plot_trajectory_bokeh(x = x, data = data, ...))
}

#' @rdname plot_trajectory
plot_trajectory_bokeh <- function(x, data,
                                  color.y = c("blue", "grey"),
                                  size.y = 6,
                                  color.yhat = c("red", "grey"),
                                  size.yhat = 6,
                                  ncol = 3,
                                  xlab = "Age (years)",
                                  ylab = "Length (SDS)",
                                  zband = TRUE,
                                  zband_range = NULL,
                                  xlim = NULL,
                                  ylim = NULL,
                                  ...) {
  if (is.null(zband_range)) {
    if (!is.null(xlim)) {
      zband_range <- xlim
    } else {
      zband_range <- range(data$x, na.rm = TRUE)
    }
  }

  plot_one_fig <- function(x) {
    k <- x$knot
    fig <- figure(xlab = xlab, ylab = ylab, ...)
    if (zband) {
      fig <- hbgd::ly_zband(fig, x = zband_range, z = -c(2.5, 2, 1, 0))
    }
    if (any(!k)) {
      fig <- fig %>%
        ly_lines(
          x = x$x[!k], y = x$y[!k],
          color = color.y[2]
        ) %>%
        ly_points(
          x = x$x[!k], x$y[!k],
          color = color.y[1], size = size.y
        )
    }

    if (any(k)) {
      fig <- fig %>%
        ly_lines(
          x = x$x[k], y = x$yhat[k],
          color = color.yhat[2]
        ) %>%
        ly_points(
          x = x$x[k], x$yhat[k],
          color = color.yhat[1], size = size.yhat
        )
    }
    return(fig)
  }

  # split since rbokeh does not support faceting
  data <- split(data, as.factor(as.character(data$subjid)), drop = TRUE)
  figs <- lapply(data, FUN = plot_one_fig)

  # put together different plots
  if (length(figs) == 1) {
    return(figs[[1]])
  }
  grid_plot(figs,
    xlim = xlim, ylim = ylim,
    same_axes = TRUE, simplify_axes = TRUE, ncol = ncol
  )
}

#' @rdname plot_trajectory
plot_trajectory_ggplot <- function(x, data,
                                   color.y = c("blue", "grey"),
                                   size.y = 2,
                                   color.yhat = c("red", "grey"),
                                   size.yhat = 2,
                                   ncol = 3,
                                   xlab = "Age (years)",
                                   ylab = "Length (SDS)",
                                   zband = TRUE,
                                   zband_range = NULL,
                                   xlim = NULL,
                                   ylim = NULL,
                                   theme = ggplot2::theme_light(),
                                   ...) {
  g <- ggplot(data, aes_string(x = "x", y = "y")) +
    xlab(xlab) +
    ylab(ylab)

  if (!is.null(xlim)) g <- g + xlim(xlim)
  if (!is.null(ylim)) g <- g + ylim(ylim)

  # zband_color Note color #59a14f is Tableau10 green
  zband_color <- "#59a14f"
  if (is.null(zband_range)) {
    if (!is.null(xlim)) {
      zband_range <- xlim
    } else {
      zband_range <- range(data$x, na.rm = TRUE)
    }
  }
  if (zband) {
    g <- hbgd::geom_zband(g,
      x = zband_range,
      z = -c(2.5, 2, 1, 0),
      color = zband_color,
      alpha = 0.15
    )
  }


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
        data = data[k, ], color = color.yhat[2]
      ) +
      geom_point(aes_string(y = "yhat"),
        data = data[k, ], color = color.yhat[1],
        size = size.yhat
      )
  }

  # split out according to subjid
  g <- g + facet_wrap(~subjid, ncol = ncol)

  # set theme
  g <- g + theme

  return(g)
}


#' Get the label of a HBGD standard variable
#'
#' This functions consults the \code{hbgd::hbgd_labels} table to find the variable
#' label.
#'
#' @note This function should be placed in the \code{hbgd} package.
#' @param x An character object with the name of one variable
#' @return A character vector, either the label found in the table, or the name of the variable.
#' @export
get_label <- function(x) {
  label <- hbgd::hbgd_labels[[x]]
  if (is.null(label)) {
    return(x)
  }
  return(label)
}
