#' Fit a `brokenstick` model to irregular data
#'
#' The `brokenstick()` function fits an irregularly observed series
#' of measurements onto a user-specified grid of points.
#' The model codes de grid by a series of linear B-splines.
#' Differences between observations are expressed by one random
#' effect per grid point. When multiple set of series are modelled,
#' each modeled trajectory consists of straight lines that join at the
#' chosen grid points, and hence look like a broken stick.
#'
#' @param x Predictor variables. Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' If `x` has one column, then also specify `y` and `group`. If `x` has multiple
#' columns, then specify the model by a `formula` argument.
#'
#' @param y Outcome variable. When `x` is a __data frame__ or __matrix__,
#' `y` is specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param group Grouping variable. When `x` is a __data frame__ or __matrix__,
#' `group` is specified as:
#'
#'   * A __data frame__ with 1 column.
#'   * A __matrix__ with 1 column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing predictor, group and outcome variables.
#'
#' @param formula A formula specifying the outcome terms on the
#' left-hand side, the predictor term on the right-hand side and
#' the group variable after the `|` sign, e.g `formula = hgt ~ age | id`.
#' One may specify additional variables, but the `brokenstick` model
#' will ignored them.
#'
#'  Note: This formula specification is specific to the `brokenstick()`
#'  function, and generates the error
#'  `No in-line functions should be used here` if passed directly to
#' `recipes::recipe()`. See examples.
#'
#' @param knots Optional, but recommended. Numerical vector with the
#' locations of the breaks to be placed on the values of the predictor.
#' Values outside the range of the data will extend the `boundary`
#' knots (see below) beyond the data range.
#'
#' @param boundary Optional, but recommended. Numerical vector of
#' length 2 with the left and right boundary knots. The `boundary`
#' setting is passed to [splines::bs()] as the `Boundary.knots` argument.
#' If not specified, then the range of predictor variable is taken. Since
#' the range depends on the data, it is recommended to specify `boundary`
#' explicitly. Note the the `boundary` range is internally expanded
#' to include at least `range(knots)`.
#'
#' @param k Optional, a convenience parameter for the number of
#' internal knots. If specified, then `k` internal knots are placed
#' at equidense quantiles of the predictor. For example,
#' specifying `k = 1` puts a knot at the 50th quantile (median),
#' setting `k = 3` puts knots at the 25th, 50th and 75th quantiles,
#' and so on.
#'
#' Note: Knots specified via `k` are data-dependent
#' and do not transfer to other data sets. We recommend
#' using `knots` and `boundary` over `k`. If both `k` and
#' `knots` are specified, then `k` takes precendence.
#'
#' @param control A list with parameters. Use `control_brokenstick()`
#' to generate.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @param seed Seed number for [base::set.seed()]. Use `NA` to bypass
#' seed setting.
#'
#' @details
#' The variance-covariance matrix of the random effects absorbs the
#' relations over time. Currently, this matrix is estimated
#' as unstructured by [lme4::lmer()].

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
#' # fit with implicit boundary c(0, 3)
#' fit <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = 0:3)
#'
#' control <- control_brokenstick(method = "kr")
#' fit <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = 0:3,
#'                    control = control)
#'
#' knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24) / 12, 4)
#' boundary <- c(0, 3)
#' fit_kr <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = knots,
#'                       boundary = boundary, control = control)
#'
#' \dontrun{
#' fit_lmer <- brokenstick(hgt.z ~ age | id, data = smocc_200,
#'                         knots = knots, boundary = boundary)
#'}
#'
#' # Five ways to specify the same model
#' # Formula interface
#' mod1 <- brokenstick(mpg ~ disp | cyl, mtcars)
#'
#' # XY interface - numeric vector
#' mod2 <- with(mtcars, brokenstick(disp, mpg, cyl))
#' identical(mod1, mod2)
#'
#' # Recipes data.frame interface
#' library(recipes)
#' rec <- recipe(mtcars,
#'               vars = c("mpg", "disp", "cyl"),
#'               roles = c("outcome", "predictor", "group"))
#' mod3 <- brokenstick(rec, mtcars)
#' identical(mod1, mod3)
#'
#' # XY interface - data.frame
#' mod4 <- with(mtcars, brokenstick(data.frame(disp), mpg, cyl))
#' identical(mod1, mod4)
#'
#' # XY interface - matrix
#' mt <- as.matrix(mtcars)
#' mod5 <- brokenstick(mt[, "disp", drop = FALSE],
#'                     mt[, "mpg", drop = FALSE],
#'                     mt[, "cyl", drop = FALSE])
#' @export
brokenstick <- function(x, ...) {
  UseMethod("brokenstick")
}

#' @export
#' @rdname brokenstick
brokenstick.default <- function(x, ...) {
  stop("`brokenstick()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}


# Formula method

#' @export
#' @rdname brokenstick
brokenstick.formula <- function(formula, data, ...,
                                knots = NULL,
                                boundary = NULL,
                                k = NULL,
                                control = control_brokenstick(),
                                seed = NA) {
  # pre-process formula to get around mold()'s formula limitations
  nms <- parse_formula(formula)
  rec <- recipes::recipe(data,
                         vars = c(nms$y, nms$x, nms$z),
                         roles = c("outcome", "predictor", "group"))
  processed <- hardhat::mold(rec, data)

  brokenstick_bridge(processed, knots, boundary, k, control, seed, ...)
}


# Recipe method

#' @export
#' @rdname brokenstick
brokenstick.recipe <- function(x, data, ...,
                               knots = NULL,
                               boundary = NULL,
                               k = NULL,
                               control = control_brokenstick(),
                               seed = NA) {

  processed <- hardhat::mold(x, data)
  brokenstick_bridge(processed, knots, boundary, k, control, seed, ...)
}


# XY method - data frame

#' @export
#' @rdname brokenstick
brokenstick.data.frame <- function(x, y, group, ...,
                                   knots = NULL,
                                   boundary = NULL,
                                   k = NULL,
                                   control = control_brokenstick(),
                                   seed = NA) {
  nms <- list(
    y = ifelse(is.null(names(y)),
               deparse(substitute(y)),
               names(y)[1L]),
    x = names(x)[1L],
    g = ifelse(is.null(names(group)),
               deparse(substitute(group)),
               names(group)[1L]))

  data <- dplyr::bind_cols(x[, 1L, drop = FALSE], y, group)
  data <- setNames(data, c(nms$x, nms$y, nms$g))

  rec <- recipes::recipe(data,
                         vars = c(nms$y, nms$x, nms$g),
                         roles = c("outcome", "predictor", "group"))
  processed <- hardhat::mold(rec, data)

  brokenstick_bridge(processed, knots, boundary, k, control, seed, ...)
}


# XY method - matrix

#' @export
#' @rdname brokenstick
brokenstick.matrix <- function(x, y, group, ...,
                               knots = NULL,
                               boundary = NULL,
                               k = NULL,
                               control = control_brokenstick(),
                               seed = NA) {
  nms <- list(
    y = ifelse(is.null(colnames(y)),
               deparse(substitute(y)),
               colnames(y)[1L]),
    x = colnames(x)[1L],
    g = ifelse(is.null(colnames(group)),
               deparse(substitute(group)),
               colnames(group)[1L]))

  data <- dplyr::bind_cols(data.frame(x[, 1L, drop = FALSE]), y, group)
  data <- setNames(data, as.character(c(nms$x, nms$y, nms$g)))

  rec <- recipes::recipe(data,
                         vars = c(nms$y, nms$x, nms$g),
                         roles = c("outcome", "predictor", "group"))
  processed <- hardhat::mold(rec, data)

  brokenstick_bridge(processed, knots, boundary, k, control, seed, ...)
}

# XY method - numeric vector

#' @export
#' @rdname brokenstick
brokenstick.numeric <- function(x, y, group, ...,
                               knots = NULL,
                               boundary = NULL,
                               k = NULL,
                               control = control_brokenstick(),
                               seed = NA) {
  nms <- list(
    y = ifelse(is.null(colnames(y)),
               deparse(substitute(y)),
               colnames(y)[1L]),
    x = ifelse(is.null(colnames(x)),
               deparse(substitute(x)),
               colnames(x)[1L]),
    g = ifelse(is.null(colnames(group)),
               deparse(substitute(group)),
               colnames(group)[1L]))

  data <- dplyr::bind_cols(x, y, group)
  data <- setNames(data, as.character(c(nms$x, nms$y, nms$g)))

  rec <- recipes::recipe(data,
                         vars = c(nms$y, nms$x, nms$g),
                         roles = c("outcome", "predictor", "group"))
  processed <- hardhat::mold(rec, data)

  brokenstick_bridge(processed, knots, boundary, k, control, seed, ...)
}


# ------------------------------------------------------------------------------
# Bridge

brokenstick_bridge <- function(processed, knots, boundary, k, control, seed, ...) {

  x <- processed$predictor
  y <- processed$outcome
  g <- processed$extras$roles$group
  nms <- list(x = names(x), y = names(y), g = names(g))
  data <- data.frame(x, y, g, stringsAsFactors = FALSE)

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x, knots = l$knots, boundary = l$boundary)

  if (control$method == "lmer") {
    data_pad <- data.frame(data, X, stringsAsFactors = FALSE)
    names(data_pad) <- c(names(data), colnames(X))
    pred <- paste("0 +", paste(colnames(X), collapse = " + "))
    fm <- as.formula(paste(nms$y, "~", pred, "+ (", pred, "|", nms$g, ")"))
    fit <- brokenstick_impl_lmer(data = data_pad,
                                 formula = fm,
                                 control = control$lmer,
                                 na.action = control$na.action)
  }
  if (control$method == "kr") {
    fit <- kr(y = y,
              x = X,
              g = g,
              control = control$kr,
              seed = seed,
              na.action = control$na.action)
  }

  new_brokenstick(
    data = data,
    names = nms,
    knots = l$knots,
    boundary = l$boundary,
    beta = fit$beta,
    omega = fit$omega,
    sigma2j = fit$sigma2j,
    sigma2 = fit$sigma2,
    draws = fit$draws,
    blueprint = processed$blueprint)
}


# ------------------------------------------------------------------------------
# Implementation

brokenstick_impl_lmer <- function(data, formula, control, na.action) {

  # Bates et al, linear mixed-effects model
  model <- lmer(formula = formula,
                data = data,
                control = control,
                na.action = na.action)

  df <- as.data.frame(VarCorr(model))
  list(
    model = model,
    beta = lme4::fixef(model),
    omega = as.matrix(as.data.frame(VarCorr(model)[[names(data)[3L]]])),
    sigma2j = numeric(),
    sigma2 = df[df$grp == "Residual", "vcov"],
    draws = numeric())
}

brokenstick_impl_kr <- function(y, x, g, control, na.action) {

  # Kasim-Raudenbush sampler
  kr(y = y,
     x = x,
     g = g,
     control = control,
     na.action = na.action)
}

