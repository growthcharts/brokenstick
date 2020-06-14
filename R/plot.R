#' Plot observed and fitted trajectories by group
#'
#' The `plot` method for a `brokenstick` object plots the observed and
#' fitted trajectories of one or more groups.
#'
#' @param x An object of class `brokenstick`.
#' @param .x The `x` argument of the [predict.brokenstick()] function.
#' @param n_plot A integer indicating the number of individual plots.
#' The default is 3, which plots the trajectories of the first three
#' groups. The `n_plot` is a safety measure to prevent unintended
#' plots of the entire data set.
#' @param x_trim A range on the x-axis that can be used to subset values
#' that are plotted.
#' @param show A logical vector of length 2. The first element specifies
#' whether the observed data are plotted, the second element specifies
#' whether the added data (e.g. knots) are plotted. The default is
#' `c(TRUE, TRUE)`.
#' @param ... Extra arguments passed down to [predict.brokenstick()]
#' and [plot_trajectory()].
#' @inheritParams predict.brokenstick
#' @return An object of class [ggplot2::ggplot].
#' @author Stef van Buuren 2020
#' @method plot brokenstick
#' @seealso [predict.brokenstick], [plot_trajectory].
#' @examples
#' library("brokenstick")
#' dat <- smocc_200
#' # fit one line model for data exploration
#' fit <- brokenstick(hgt ~ age | id, dat)
#'
#' # plot measurements for first three cases
#' # plot(fit_200, smocc_200, zband = FALSE, show = c(TRUE, FALSE))
#'
#' # fit model with knots at 1 and 2 years
#' #fit <- fit_brokenstick(dat, hgt.z ~ age | id, knots = 1:2)
#'
#' #plot(fit, xlim = c(0, 2.2), ylim = c(-3, 3))
#' #plot(fit, ids = c(10005, 10012), xlim = c(0, 2.2))
#'
#' # bokeh plots with xlim and ylim
#' #plot(fit, xlim = c(0, 2), ylim = c(-3, 3))
#' #plot(fit, ids = 10005)
#' @export
plot.brokenstick <- function(x,
                             new_data,
                             ...,
                             .x = NULL,
                             group = NULL,
                             n_plot = 3,
                             x_trim = c(-Inf, Inf),
                             show = c(TRUE, TRUE)) {
  if (!inherits(x, "brokenstick")) stop("Argument `x` not of class brokenstick.")

  # calculate brokenstick predictions, long format
  if (show[2L] && missing(.x)) .x <- "knots"
  data <- predict(object = x, new_data = new_data, x = .x,
                      group = group, strip_data = FALSE, ...)

  # apply trim
  idx <- data[[x$names$x]] >= x_trim[1L] & data[[x$names$x]] <= x_trim[2L]
  data <- data[idx, , drop = FALSE]

  plot_trajectory(x = x, data = data, ...)
}


#' Plot observed and fitted trajectories from fitted brokenstick model
#'
#' This function plot the observed and fitted trajectories from brokenstick model.
#' Actual plotting may be done by `gglot`.
#' @param x An object of class `brokenstick`.
#' @param data A `data.frame` with columns names `x$names`, `".source"`
#' and `".pred"`.
#' @param color_y A character vector with two elements specifying the symbol and line color of the measured data points
#' @param size_y Dot size of measured data points
#' @param color_yhat A character vector with two elements specifying the symbol and line color of the predicted data points
#' @param size_yhat Dot size of predicted data points
#' @param ncol Number of columns in plot
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param xlim Vector of length 2 with range of x-axis
#' @param ylim Vector of length 2 with range of y-axis
#' @param theme Plotting theme
#' @param zband A logical indicating whether the Z-score band should be
#' added to the plot. The default is `TRUE`.
#' @param zband_range a vector specifying the range (min, max) that the superposed growth standard should span on the x-axis. The
#' default is the entire data range.
#' @return An object of class \code{ggplot}
#' @rdname plot_trajectory
#' @seealso [plot.brokenstick]
#' @examples
#' # plot first three cases
#' # plot(fit_200, brokenstick::smocc_200)
#' @export
plot_trajectory  <- function(x, data,
                             color_y = c("blue", "grey"),
                             size_y = 2,
                             color_yhat = c("red", "grey"),
                             size_yhat = 2,
                             ncol = 3L,
                             xlab = "Age (years)",
                             ylab = "Length (SDS)",
                             zband = TRUE,
                             zband_range = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             theme = ggplot2::theme_light()) {

  g <- ggplot(data, aes_string(x = x$names$x, y = x$names$y)) +
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
      zband_range <- range(data[[x$names$x]], na.rm = TRUE)
    }
  }
  if (zband) {
    g <- geom_zband(g,
                    x = zband_range,
                    z = -c(2.5, 2, 1, 0),
                    color = zband_color,
                    alpha = 0.15
    )
  }

  # add observed data points and lines
  k <- data$.source == "added"
  if (any(!k)) {
    g <- g +
      geom_line(data = data[!k, ], color = color_y[2L]) +
      geom_point(data = data[!k, ], color = color_y[1L], size = size_y)
  }

  # add broken stick points and lines
  if (any(k)) {
    g <- g +
      geom_line(aes_string(y = ".pred"),
                data = data[k, ], color = color_yhat[2L]
      ) +
      geom_point(aes_string(y = ".pred"),
                 data = data[k, ], color = color_yhat[1L],
                 size = size_yhat
      )
  }

  # split out according to subjid
  g <- g +
    facet_wrap(as.formula(paste("~", x$names$g)), ncol = ncol) +
    theme

  return(g)
}
