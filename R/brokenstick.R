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
#' @param data When a __formula__ is used, `data` is specified as:
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
#'  function.
#'
#' @param knots Optional, but recommended. Numerical vector with the
#' locations of the breaks to be placed on the values of the predictor.
#' Values outside the range of the data will extend the `boundary`
#' knots (see below) beyond the data range.
#'
#' @param boundary Optional. Numerical vector of
#' length 2 with the left and right boundary knots. The `boundary`
#' setting is passed to [splines::bs()] as the `Boundary.knots` argument.
#' If not specified, the range of predictor variable is taken. Automatic
#' model specification is data-dependent. However, if both `knots` and
#' `boundary` are supplied, the B-spline transformation parameter do not
#' depend on the data. If specified, the `boundary` range is internally
#' expanded to include at least `range(knots)`. The warning
#' `some 'x' values beyond boundary knots may cause ill-conditioned bases`
#' implies that model fitting ignores any data beyond the (expanded) boundary
#' range. It is possible to prevent this warning by pre-filtering rows
#' in `data` to the boundary range.
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
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is `degree = 1`.
#' Setting `degree = 0` yields (crisp) dummy coding, and one
#' column less than for `degree = 1`. The `brokenstick` package supports
#' only `degree = 0` and `degree = 1`.
#'
#' @param method Estimation method. Either `"kr"` (for the
#' Kasim-Raudenbush sampler) or `"lmer"` (for [lme4::lmer()]) (default).
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
#' relations over time. By default, this matrix is estimated
#' as unstructured by [lme4::lmer()].
#' This estimate may be unstable if
#' the number of children is small relative to the number of specified
#' knots. The function can be time consuming for data sets with thousands of
#' children.
#'
#' An alternative - often faster for models with many random effects -
#' is to use the Bayesian Kasim-Raudenbush sampler (method `kr`). That
#' method also allow for enforcing a simple structure on the
#' variance-covariance matrix of the random effects. Currently, there
#' are three such models: `argyle`, `cole` and `none`. See [kr()] and
#' [control_brokenstick()] for more details.
#'
#' @return
#'
#' A `brokenstick` object.
#'
#' @examples
#' train <- smocc_200[1:1198, ]
#'
#' # fit with implicit boundary c(0, 3)
#' fit <- brokenstick(hgt.z ~ age | id, data = train, knots = 0:3)
#'
#' \dontrun{
#' # using KR sampler
#' fit <- brokenstick(hgt.z ~ age | id, data = train, knots = 0:3,
#'                    method = "kr", seed = 1)
#'
#' knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24) / 12, 4)
#' boundary <- c(0, 3)
#' fit_lmer <- brokenstick(hgt.z ~ age | id, data = train,
#'                         knots = knots, boundary = boundary)
#' fit_kr <- brokenstick(hgt.z ~ age | id, data = train, knots = knots,
#'                       boundary = boundary, method = "kr")
#'}
#'
#' # Four ways to specify the same model
#' # Formula interface
#' mod1 <- brokenstick(hgt.z ~ age | id, train)
#'
#' # XY interface - numeric vector
#' mod2 <- with(train, brokenstick(age, hgt.z, id))
#' identical(mod1, mod2)
#'
#' # XY interface - data.frame
#' mod3 <- with(train, brokenstick(data.frame(age), hgt.z, id))
#' identical(mod1, mod3)
#'
#' # XY interface - matrix
#' tt <- as.matrix(train[, c(1, 2, 7)])
#' mod4 <- brokenstick(tt[, "age", drop = FALSE],
#'                     tt[, "hgt.z", drop = FALSE],
#'                     tt[, "id", drop = FALSE])
#' identical(mod1, mod4)
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
                                degree = 1L,
                                method = c("lmer", "kr"),
                                control = control_brokenstick(),
                                seed = NA) {
  method <- match.arg(method)

  # pre-process formula to get around mold()'s formula limitations
  nms <- parse_formula(formula)
  brokenstick_bridge(data, nms, knots, boundary, k, degree, method, control, seed, ...)
}


# XY method - data frame

#' @export
#' @rdname brokenstick
brokenstick.data.frame <- function(x, y, group, ...,
                                   knots = NULL,
                                   boundary = NULL,
                                   k = NULL,
                                   degree = 1L,
                                   method = c("lmer", "kr"),
                                   control = control_brokenstick(),
                                   seed = NA) {
  method <- match.arg(method)
  nms <- list(
    y = ifelse(is.null(names(y)),
               deparse(substitute(y)),
               names(y)[1L]),
    x = names(x)[1L],
    g = ifelse(is.null(names(group)),
               deparse(substitute(group)),
               names(group)[1L]))

  data <- data.frame(x[, 1L, drop = FALSE], y, group)
  data <- setNames(data, c(nms$x, nms$y, nms$g))

  brokenstick_bridge(data, nms, knots, boundary, k, degree, method, control, seed, ...)
}


# XY method - matrix

#' @export
#' @rdname brokenstick
brokenstick.matrix <- function(x, y, group, ...,
                               knots = NULL,
                               boundary = NULL,
                               k = NULL,
                               degree = 1L,
                               method = c("lmer", "kr"),
                               control = control_brokenstick(),
                               seed = NA) {
  method <- match.arg(method)
  nms <- list(
    y = ifelse(is.null(colnames(y)),
               deparse(substitute(y)),
               colnames(y)[1L]),
    x = colnames(x)[1L],
    g = ifelse(is.null(colnames(group)),
               deparse(substitute(group)),
               colnames(group)[1L]))

  data <- data.frame(x[, 1L, drop = FALSE], y, group)
  data <- setNames(data, as.character(c(nms$x, nms$y, nms$g)))

  brokenstick_bridge(data, nms, knots, boundary, k, degree, method, control, seed, ...)
}

# XY method - numeric vector

#' @export
#' @rdname brokenstick
brokenstick.numeric <- function(x, y, group, ...,
                                knots = NULL,
                                boundary = NULL,
                                k = NULL,
                                degree = 1L,
                                method = c("lmer", "kr"),
                                control = control_brokenstick(),
                                seed = NA) {
  method <- match.arg(method)
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

  data <- data.frame(x, y, group)
  data <- setNames(data, as.character(c(nms$x, nms$y, nms$g)))

  brokenstick_bridge(data, nms, knots, boundary, k, degree, method, control, seed, ...)
}


# ------------------------------------------------------------------------------
# Bridge

brokenstick_bridge <- function(data, nms, knots, boundary, k, degree,
                               method, control, seed, ...) {

  if (!degree %in% c(0, 1)) stop("brokenstick supports only degree 0 or 1", call. = FALSE)

  y <- data[[nms$y]]
  x <- data[, nms$x, drop = FALSE]
  g <- data[[nms$g]]

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x, knots = l$knots, boundary = l$boundary,
                  degree = degree)

  if (method == "lmer") {
    data_pad <- data.frame(data, X, stringsAsFactors = FALSE)
    names(data_pad) <- c(names(data), colnames(X))
    pred <- paste("0 +", paste(colnames(X), collapse = " + "))
    fm <- as.formula(paste(nms$y, "~", pred, "+ (", pred, "|", nms$g, ")"))
    fit <- brokenstick_impl_lmer(data = data_pad,
                                 formula = fm,
                                 control = control$lmer,
                                 na.action = control$na.action)
  }
  if (method == "kr") {
    fit <- kr(y = y,
              x = X,
              g = g,
              control = control$kr,
              seed = seed,
              na.action = control$na.action)
  }

  new_brokenstick(
    names = nms,
    knots = l$knots,
    boundary = l$boundary,
    degree = degree,
    method = method,
    control = control,
    beta = fit$beta,
    omega = fit$omega,
    sigma2j = fit$sigma2j,
    sigma2 = fit$sigma2,
    draws = fit$draws)
}


# ------------------------------------------------------------------------------
# Implementation

brokenstick_impl_lmer <- function(data, formula, control, na.action) {

  # Bates et al, linear mixed-effects model
  model <- lmer(formula = formula,
                data = data,
                control = control,
                na.action = na.action)

  # Here we trust that names(slot(model, "cnms")) gives the name of the
  # group variable
  df <- as.data.frame(VarCorr(model))
  list(
    model = model,
    beta = lme4::fixef(model),
    omega = as.matrix(as.data.frame(VarCorr(model)[[names(slot(model, "cnms"))]])),
    sigma2j = numeric(),
    sigma2 = df[df$grp == "Residual", "vcov"],
    draws = numeric())
}

brokenstick_impl_kr <- function(y, x, g, control, seed, na.action) {

  # Kasim-Raudenbush sampler
  kr(y = y,
     x = x,
     g = g,
     control = control,
     seed = seed,
     na.action = na.action)
}

