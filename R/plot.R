#' Plot observed and fitted trajectories by group
#'
#' The `plot` method for a `brokenstick` object plots the observed and
#' fitted trajectories of one or more groups.
#'
#' @param x An object of class `brokenstick`.
#' @param .x The `x` argument of the [predict.brokenstick()] function.
#' @param xlim Vector of length 2 with range of x-axis
#' @param ylim Vector of length 2 with range of y-axis
#' @param show A logical vector of length 2. The first element specifies
#' whether the observed data are plotted, the second element specifies
#' whether the added data (e.g. knots) are plotted. The default is
#' `c(TRUE, TRUE)`.
#' @param n_plot A integer indicating the number of individual plots.
#' The default is 3, which plots the trajectories of the first three
#' groups. The `n_plot` is a safety measure to prevent unintended
#' plots of the entire data set.
#' @param ... Extra arguments passed down to [predict.brokenstick()]
#' and [plot_trajectory()].
#' @inheritParams predict.brokenstick
#' @return An object of class [ggplot2::ggplot].
#' @author Stef van Buuren 2020
#' @method plot brokenstick
#' @seealso [predict.brokenstick], [plot_trajectory].
#' @examples
#' \dontrun{
#' # fit model on raw hgt with knots at 0, 1, 2 and 3 years
#' fit1 <- brokenstick(hgt ~ age | id, smocc_200, knots = 0:3)
#' gp <- c(10001, 10005, 10022)
#' plot(fit1, smocc_200, group = gp, xlim = c(0, 2.1),
#'      xlab = "Age (years)", ylab = "Length (cm)")
#'
#' # fit model on standard deviation score
#' fit2 <- brokenstick(hgt.z ~ age | id, smocc_200, knots = 0:3)
#' plot(fit2, smocc_200, group = gp, xlim = c(0, 2.1),
#'      xlab = "Age (years)", ylab = "Length (SDS)")
#'
#' # model with 11 knots
#' plot(fit_200, smocc_200, group = gp, xlim = c(0, 2.1),
#'      xlab = "Age (years)", ylab = "Length (SDS)")
#' }
#' @export
plot.brokenstick <- function(x,
                             new_data,
                             ...,
                             .x = NULL,
                             group = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             show = c(TRUE, TRUE),
                             n_plot = 3L) {
  if (!inherits(x, "brokenstick")) stop("Argument `x` not of class brokenstick.")
  if (!any(show)) stop("At least one of `show` should be TRUE.")

  # calculate brokenstick predictions, long format
  if (show[2L] && missing(.x)) .x <- "knots"
  data <- predict(object = x, new_data = new_data, x = .x,
                  group = group, strip_data = FALSE, ...)
  if (ncol(data) == 1L) data <- bind_cols(.source = "data", new_data, data)

  # apply trims
  idx <- rep(TRUE, nrow(data))
  idx <- idx &
    !is.na(data[[x$names$x]]) &
    !(is.na(data[[x$names$y]]) & data[[".source"]] == "data")
  if (!is.null(xlim))
    idx <- idx & data[[x$names$x]] >= xlim[1L] & data[[x$names$x]] <= xlim[2L]
  if (!is.null(ylim))
    idx <- idx &
    ((data[[".source"]] == "data" &
       data[[x$names$y]] >= ylim[1L] &
       data[[x$names$y]] <= ylim[2L]) |
    (data[[".source"]] == "added" &
       data[[".pred"]] >= ylim[1L] &
       data[[".pred"]] <= ylim[2L]))

  # safety measure, restrict to first n_plot cases if no groups are specified
  if (is.null(group)) {
    ids <- unique(data[[x$names$g]])
    idx <- idx & (data[[x$names$g]] %in% ids[1L:min(n_plot, length(ids))])
  }

  # process show vector
  if (!show[1L]) idx <- idx & data[[".source"]] != "data"
  if (!show[2L]) idx <- idx & data[[".source"]] != "added"

  data <- data[idx, , drop = FALSE]
  plot_trajectory(x = x, data = data, xlim = xlim, ylim = ylim, ...)
}


#' Plot observed and fitted trajectories from fitted brokenstick model
#'
#' This function is called by `plot.brokenstick`. Normally we wouldn't
#' call `plot_trajectory()` directly.
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
#' @param scales Axis scaling, e.g. `"fixed"`, `"free"`, and so on
#' @param theme Plotting theme
#' @return An object of class \code{ggplot}
#' @rdname plot_trajectory
#' @seealso [plot.brokenstick]
plot_trajectory  <- function(x,
                             data,
                             color_y = c(grDevices::hcl(240, 100, 40, 0.7),
                                         grDevices::hcl(240, 100, 40, 0.8)),
                             size_y = 2,
                             color_yhat = c(grDevices::hcl(0, 100, 40, 0.7),
                                            grDevices::hcl(0, 100, 40, 0.8)),
                             size_yhat = 2,
                             ncol = 3L,
                             xlab = NULL,
                             ylab = NULL,
                             xlim = NULL,
                             ylim = NULL,
                             scales = "fixed",
                             theme = ggplot2::theme_light()) {

  if (is.null(xlab)) xlab <- x$names$x
  if (is.null(ylab)) ylab <- x$names$y
  g <- ggplot(data, aes_string(x = x$names$x, y = x$names$y)) +
    xlab(xlab) +
    ylab(ylab)

  if (!is.null(xlim)) g <- g + xlim(xlim)
  if (!is.null(ylim)) g <- g + ylim(ylim)

  # add observed data points and lines
  k <- data$.source == "added"
  if (any(!k)) {
    g <- g +
      geom_line(data = data[!k, ], color = color_y[2L]) +
      geom_point(data = data[!k, ], color = color_y[1L], size = size_y)
  }

  # add broken stick points and lines
  if (any(k)) {
    if (x$degree == 0L) {
      g <- g + geom_step(aes_string(y = ".pred"),
                         data = data[k, ], color = color_yhat[2L])
    } else {
      g <- g +
        geom_line(aes_string(y = ".pred"),
                  data = data[k, ], color = color_yhat[2L]) +
        geom_point(aes_string(y = ".pred"),
                   data = data[k, ], color = color_yhat[1L],
                   size = size_yhat)
    }
  }

  # split out according to subjid
  g <- g +
    facet_wrap(as.formula(paste("~", x$names$g)),
               ncol = ncol,
               scales = scales) +
    theme

  g
}
