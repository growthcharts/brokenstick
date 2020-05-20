#' Fit a `brokenstick` model to irregular data
#'
#' `brokenstick()` fits an irregularly observed series
#' of measurements onto a user-specified grid of points.
#' The model codes de grid by a series of linear B-splines.
#' Differences between observations are expressed by one random
#' effect per grid point. When multiple set of series are modelled,
#' each modeled trajectory consists of straight lines that join at the
#' chosen grid points, and hence look like a 'broken stick'.
#'
#' @param x Predictor variables. Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' The current model accepts only one predictor (by
#' default the first column in __data frame__ or __matrix__) and only
#' one group variable (by default the last column in
#' __data frame__ or __matrix__).
#'
#' @param y Outcome variable. When `x` is a __data frame__ or __matrix__,
#' `y` is specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing predictor, group and outcome variables.
#'
#' @param formula A formula specifying the outcome terms on the
#' left-hand side, the predictor term on the right-hand side and
#' the group variable after the `|` sign, e.g `formula = hgt ~ age | id`.
#'
#' @param knots optional, numerical vector with the locations of the breaks to be
#' placed on the values of the predictor. Values outside the range of the data
#' will extend the `boundary` knots (see below) beyond the data range.
#'
#' @param boundary optional, numerical vector of length 2 with the left and
#' right boundary knots. The `boundary` setting is passed to
#' __splines::bs()__ as the `Boundary.knots` argument. If not specified,
#' then the range of predictor variable is taken. Since the range
#' depends on the data, it is recommended to specify `boundary` explicitly.
#' Note the the `boundary` range is internally expanded to include at
#' least `range(knots)`.
#'
#' @param k optional, a convenience parameter giving the number of
#' internal knots. If specified, then `k` internal knots are placed
#' at equidense quantiles of the predictor. For example,
#' specifying `k = 1` puts a knot at the 50th quantile (median),
#' setting `k = 3` puts knots at the 25th, 50th and 75th quantiles,
#' and so on. Note that knots specified via `k` are data-dependent
#' and do not transfer well to other data sets. We therefore recommend
#' using `knots` and `boundary` over `k`. If both `k`` and
#' `knots` are specified, then `k` take precendence. This is likely
#' to change in the future.
#'
#' @param method Either `"kr"` (for the Kasim-Raudenbush sampler)
#' or `"lmer"` (for __lme4::lmer__).
#'
#' @param control A list with arguments that can be used to control the
#' fitting of __lme4::lmer()__. The default
#' is set to `lmerControl(check.nobs.vs.nRE = "warning")`, which turn
#' fatal errors with respect the number of parameters into warnings.
#'
#' @param na.action The function to call for the `na.action` argument
#' in `lmer()`. The default is `na.exclude`.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @details
#' The variance-covariance matrix of the random effects absorbs the
#' relations over time. Currently, this matrix is estimated
#' as unstructured by `lmer()` from the `lme4` package.

#' This estimate may be unstable if
#' the number of children is small relative to the number of specified
#' knots. The function can be time consuming for data sets with thousands of
#' children.
#'
#' @return
#'
#' A `brokenstick` object.
#'
#' @examples
#' # data <- brokenstick::smocc_200
#'
#' # fit with implicit boundary c(0, 3)
#' # fit <- with(data, brokenstick(y = hgt.z, x = age, subjid = id, knots = 0:3))
#'
#' \dontrun{
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- brokenstick(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- brokenstick(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- brokenstick(rec, mtcars)
#' }
#' @export
brokenstick <- function(x, ...) {
  UseMethod("brokenstick")
}

#' @export
#' @rdname brokenstick
brokenstick.default <- function(x,
                                ...,
                                knots = NULL,
                                boundary = NULL,
                                k = NULL,
                                subset = NULL,
                                weights = NULL,
                                na.action = na.exclude,
                                method = c("lmer", "kr", "model.frame"),
                                control = list()) {
  stop("`brokenstick()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname brokenstick
brokenstick.data.frame <- function(x, y, ...,
                                   knots = NULL,
                                   boundary = NULL,
                                   k = NULL,
                                   subset = NULL,
                                   weights = NULL,
                                   na.action = na.exclude,
                                   method = c("lmer", "kr", "model.frame"),
                                   control = list()) {
  processed <- hardhat::mold(x, y)
  brokenstick_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname brokenstick
brokenstick.matrix <- function(x, y, ...,
                               knots = NULL,
                               boundary = NULL,
                               k = NULL,
                               subset = NULL,
                               weights = NULL,
                               na.action = na.exclude,
                               method = c("lmer", "kr", "model.frame"),
                               control = list()) {
  processed <- hardhat::mold(x, y)
  brokenstick_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname brokenstick
brokenstick.formula <- function(formula, data, ...,
                                knots = NULL,
                                boundary = NULL,
                                k = NULL,
                                subset = NULL,
                                weights = NULL,
                                na.action = na.exclude,
                                method = c("lmer", "kr", "model.frame"),
                                control = list()) {
  processed <- hardhat::mold(formula, data)
  brokenstick_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname brokenstick
brokenstick.recipe <- function(x, data, ...,
                               knots = NULL,
                               boundary = NULL,
                               k = NULL,
                               subset = NULL,
                               weights = NULL,
                               na.action = na.exclude,
                               method = c("lmer", "kr", "model.frame"),
                               control = list()) {
  processed <- hardhat::mold(x, data)
  brokenstick_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

brokenstick_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  fit <- brokenstick_impl(predictors, outcome)

  new_brokenstick(
    data = dplyr::bind_cols(outcome, predictors),
    model = fit,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

brokenstick_impl <- function(predictors, outcome) {
  list(coefs = 1)
}


calc_brokenstick <- function(y, x, subjid,
                        k = NULL,
                        knots = NULL,
                        boundary = NULL,
                        method = c("lmer", "kr"),
                        control = lmerControl(check.nobs.vs.nRE = "warning"),
                        na.action = na.exclude,
                        ...) {
  .Deprecated("fit_brokenstick")

  call <- match.call()
  method <- match.arg(method)

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x = x, knots = l$knots, boundary = l$boundary)
  data <- data.frame(subjid = subjid, x = x, y = y, X)

  if (method == "lmer") {
    pred <- paste("0 +", paste(colnames(X), collapse = " + "))
    f <- as.formula(paste(
      "y", "~", pred,
      "+ (", pred, "| subjid)"
    ))
    model <- lmer(f,
                  data = data,
                  control = control,
                  na.action = na.action,
                  ...
    )

    fit <- list(
      model = model,
      knots = as.numeric(l$knots),
      boundary = as.numeric(l$boundary),
      degree = 1,
      bs.call = call,
      xy = data[, 1:3]
    )
    class(fit) <- c("brokenstick")
  }
  else
  {# kr sampler
    model <- kr(y = data$y,
                ry = !is.na(data$y),
                x = data[, c("group", colnames(X))],
                type = c(-2, rep(2, ncol(X))),
                intercept = FALSE,
                ...)
    fit <- list(
      model = NA,
      beta = model$mu,
      omega = solve(model$inv.psi),
      sigma2 = mean(1/model$inv.sigma2),
      sigma2j = 1/model$inv.sigma2,
      knots = as.numeric(l$knots),
      boundary = as.numeric(l$boundary),
      degree = 1,
      bs.call = call,
      xy = data[, 1:3])
    class(fit) <- "brokenstick"
  }
  fit
}

print.brokenstick <- function(x, ...) {
  cat("broken stick model \n")
  cat("knots: ", get_knots(x), "\n")
  print(summary(x))
  invisible(x)
}


