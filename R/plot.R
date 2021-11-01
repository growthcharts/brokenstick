#' Plot observed and fitted trajectories by group
#'
#' The `plot` method for a `brokenstick` object plots the observed and
#' fitted trajectories of one or more groups.
#'
#' By default, `plot(fit)` will plot the observed and fitted data for the
#' first three groups in the data. The default setting drops the fitted value
#' at the right boundary knot from the display.
#'
#' @param x An object of class `brokenstick`.
#' @param ... Extra arguments passed down to [predict.brokenstick()]
#' and [plot_trajectory()].
#' @inheritParams predict.brokenstick
#' @return An object of class [ggplot2::ggplot].
#' @author Stef van Buuren 2021
#' @method plot brokenstick
#' @seealso [predict.brokenstick], [plot_trajectory].
#' @examples
#' # fit model on raw hgt with knots at 0, 1, 2 and 3 years
#' fit1 <- brokenstick(hgt ~ age | id, smocc_200, knots = 0:3)
#' gp <- c(10001, 10005, 10022)
#' plot(fit1, group = gp, xlab = "Age (years)", ylab = "Length (cm)")
#'
#' # fit model on standard deviation score
#' fit2 <- brokenstick(hgt_z ~ age | id, smocc_200, knots = 0:3)
#' plot(fit2, group = gp, xlab = "Age (years)", ylab = "Length (SDS)")
#'
#' # built-in model with 11 knots
#' plot(fit_200, group = gp, xlab = "Age (years)", ylab = "Length (SDS)")
#' @export
plot.brokenstick <- function(x,
                             newdata = NULL,
                             ...) {
  install.on.demand("ggplot2", ...)
  newdata <- get_newdata(x, newdata)
  nms <- unname(unlist(x$names))
  if (!all(nms %in% colnames(newdata))) {
    stop("Variable(s) not found: ",
         paste(nms[!nms %in% colnames(newdata)], collapse = ", "),
         call. = FALSE)
  }

  g <- plot_trajectory(x = x, newdata = newdata, ...)
  return(g)
}


#' Plot observed and fitted trajectories from fitted brokenstick model
#'
#' @inheritParams predict.brokenstick
#' @param x An object of class `brokenstick`.
#' @param newdata A `data.frame` or `matrix`
#' @param what Which knots to plot? See [get_knots()]. The default,
#' `what = "droplast"`, does not plot the right boundary knot.
#' @param .x The `x` argument of the [predict.brokenstick()] function.
#' @param color_y A character vector with two elements specifying the symbol and line color of the measured data points
#' @param size_y Dot size of measured data points
#' @param color_yhat A character vector with two elements specifying the symbol and line color of the predicted data points
#' @param size_yhat Dot size of predicted data points
#' @param color_imp A character vector with two elements specifying the symbol and line color of the imputed data
#' @param size_imp Dot size of imputed data
#' @param ncol Number of columns in plot
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param xlim Vector of length 2 with range of x-axis
#' @param ylim Vector of length 2 with range of y-axis
#' @param show A logical vector of length 3. Element 1 specifies
#' whether the observed data are plotted, element 2 specifies
#' whether the broken stick are plotted, element 3 specifies
#' whether imputations are plotted. The default is
#' `c(TRUE, TRUE, FALSE)`.
#' @param n_plot A integer indicating the number of individual plots.
#' The default is 3, which plots the trajectories of the first three
#' groups. The `n_plot` is a safety measure to prevent unintended
#' plots of the entire data set.
#' @param scales Axis scaling, e.g. `"fixed"`, `"free"`, and so on
#' @param theme Plotting theme
#' @param \dots Extra arguments passed down to [predict.brokenstick()].
#' @return An object of class \code{ggplot}
#' @rdname plot_trajectory
#' @seealso [plot.brokenstick]
#' @export
plot_trajectory <- function(x,
                            newdata,
                            what = "droplast",
                            .x = NULL,
                            group = NULL,
                            color_y = c(
                              grDevices::hcl(240, 100, 40, 0.7),
                              grDevices::hcl(240, 100, 40, 0.8)
                            ),
                            size_y = 2,
                            color_yhat = c(
                              grDevices::hcl(0, 100, 40, 0.7),
                              grDevices::hcl(0, 100, 40, 0.8)
                            ),
                            size_yhat = 2,
                            color_imp = c("grey80", "grey80"),
                            size_imp = 2,
                            ncol = 3L,
                            xlab = NULL,
                            ylab = NULL,
                            xlim = NULL,
                            ylim = NULL,
                            show = c(TRUE, TRUE, FALSE),
                            n_plot = 3L,
                            scales = "fixed",
                            theme = ggplot2::theme_light(),
                            ...) {
  stopifnot(inherits(x, "brokenstick"),
            any(show))
  # calculate brokenstick predictions, long format
  if (show[2L] && missing(.x)) .x <- "knots"
  data <- predict(
    object = x, newdata = newdata, what = what, ...,
    x = .x, group = group, strip_data = FALSE
  )
  if (ncol(data) == 1L) data <- bind_cols(.source = "data", newdata, data)

  # add imputations
  if (show[3L]) {
    if (!length(x$imp)) {
      stop("Cannot find imputations. Use method 'kr' and set 'nimp'.")
    }

    newdata_isna <- is.na(newdata[, x$names$y, drop = TRUE])
    if (sum(newdata_isna) != nrow(x$imp)) {
      stop(
        "Missing data count mismatch: ", sum(newdata_isna),
        " (newdata) versus ", nrow(x$imp), " (x$imp)."
      )
    }

    imp <- x$imp
    colnames(imp) <- paste(".imp", 1:ncol(imp), sep = "_")
    imputed <- newdata %>%
      filter(newdata_isna) %>%
      select(- x$names$y) %>%
      bind_cols(as.data.frame(imp)) %>%
      mutate(.source = "imputed") %>%
      pivot_longer(
        cols = starts_with(".imp_"),
        names_prefix = ".imp_",
        names_to = ".imp",
        names_transform = list(.imp = as.integer),
        values_to = x$names$y
      )
    data <- bind_rows(data, imputed)
  }

  # apply trims
  idx <- rep(TRUE, nrow(data))
  idx <- idx &
    !is.na(data[[x$names$x]]) &
    !(is.na(data[[x$names$y]]) & data[[".source"]] == "data")
  if (!is.null(xlim)) {
    idx <- idx & data[[x$names$x]] >= xlim[1L] & data[[x$names$x]] <= xlim[2L]
  }
  if (!is.null(ylim)) {
    idx <- idx &
      ((data[[".source"]] == "data" &
          data[[x$names$y]] >= ylim[1L] &
          data[[x$names$y]] <= ylim[2L]) |
         (data[[".source"]] == "added" &
            data[[".pred"]] >= ylim[1L] &
            data[[".pred"]] <= ylim[2L]) |
         (data[[".source"]] == "imputed" &
            data[[x$names$y]] >= ylim[1L] &
            data[[x$names$y]] <= ylim[2L]))
  }

  # safety measure, restrict to first n_plot cases if no groups are specified
  if (is.null(group)) {
    ids <- unique(data[[x$names$g]])
    group <- ids[1L:min(n_plot, length(ids))]
  }
  idx <- idx & (data[[x$names$g]] %in% group)

  # process show vector
  if (!show[1L]) idx <- idx & data[[".source"]] != "data"
  if (!show[2L]) idx <- idx & data[[".source"]] != "added"
  if (!show[3L]) idx <- idx & data[[".source"]] != "imputed"

  data <- data[idx, , drop = FALSE]
  # g <- plot_trajectory(x = x, data = data, xlim = xlim, ylim = ylim, ...)

  if (is.null(xlab)) xlab <- x$names$x
  if (is.null(ylab)) ylab <- x$names$y
  g <- ggplot2::ggplot(data, ggplot2::aes_string(x = x$names$x, y = x$names$y)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if (!is.null(xlim)) g <- g + ggplot2::xlim(xlim)
  if (!is.null(ylim)) g <- g + ggplot2::ylim(ylim)

  # add imputed data
  k <- data$.source == "imputed"
  if (any(k)) {
    g <- g +
      ggplot2::geom_line(ggplot2::aes_string(group = ".imp"),
                         data = data[k, ], color = color_imp[2L]) +
      ggplot2::geom_point(ggplot2::aes_string(y = "hgt_z"),
                          data = data[k, ], color = color_imp[1L],
                          size = size_imp)
  }

  # add observed data points and lines
  k <- data$.source == "data"
  if (any(k)) {
    g <- g +
      ggplot2::geom_line(data = data[k, ], color = color_y[2L]) +
      ggplot2::geom_point(data = data[k, ], color = color_y[1L], size = size_y)
  }

  # add broken stick points and lines
  k <- data$.source == "added"
  if (any(k)) {
    if (x$degree == 0L) {
      g <- g + ggplot2::geom_step(ggplot2::aes_string(y = ".pred"),
                                  data = data[k, ], color = color_yhat[2L]
      )
    } else {
      g <- g +
        ggplot2::geom_line(ggplot2::aes_string(y = ".pred"),
                           data = data[k, ], color = color_yhat[2L]
        ) +
        ggplot2::geom_point(ggplot2::aes_string(y = ".pred"),
                            data = data[k, ], color = color_yhat[1L],
                            size = size_yhat
        )
    }
  }

  # split out according to subjid
  g <- g +
    ggplot2::facet_wrap(as.formula(paste("~", x$names$g)),
                        ncol = ncol,
                        scales = scales
    ) +
    theme

  return(g)
}
