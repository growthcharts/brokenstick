#' Fit a `brokenstick` model to irregular data
#'
#' The `brokenstick()` function fits an irregularly observed series
#' of measurements onto a user-specified grid of points (knots).
#' The model codes the grid by a series of linear B-splines.
#' Each modelled trajectory consists of straight lines that join at
#' the chosen knots and look like a broken stick. Differences between
#' observations are expressed by a random effect per knot.
#'
#' @param formula A formula specifying the outcome, the predictor and the group
#' variable in `data`. The generic shape is `formula = y ~ x | group`. The
#' left-hand side is the outcome, the right-hand side the predictor, and the
#' name of the grouping variable occurs after the `|` sign. Formula treatment
#' is non-standard: 1) `y` and `x` should be numeric, 2) only one variable
#' is allowed in each model term (additional variables will be ignored).
#'
#' @param data A data frame or matrix containing the outcome (numeric),
#' predictor (numeric) and group (numeric, factor, character) variable.
#'
#' @param knots Optional, but recommended. Numerical vector with the
#' locations of the internal knots to be placed on the values of the predictor.
#'
#' @param boundary Optional, but recommended. Numerical vector of
#' length 2 with the left and right boundary knot. The `boundary`
#' setting is passed to [splines::bs()] as the `Boundary.knots` argument.
#' If not specified, the function determines the boundary knots as
#' `range(x)`. When specified, the `boundary` range is internally
#' expanded to include at least `range(knots)`.
#'
#' @param k Optional, a convenience parameter for the number of
#' internal knots. If specified, then `k` internal knots are placed
#' at equidense quantiles of the predictor. For example,
#' specifying `k = 1` puts a knot at the 50th quantile (median),
#' setting `k = 3` puts knots at the 25th, 50th and 75th quantiles,
#' and so on. If the user specifies both `k` and `knots` arguments
#' then `knots` takes precedence.
#'
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is `degree = 1`.
#' Setting `degree = 0` yields (crisp) dummy coding, and one
#' column less than for `degree = 1`. The `brokenstick` package supports
#' only `degree = 0` and `degree = 1`.
#'
#' @param method Estimation method. Either `"kr"` (for the
#' Kasim-Raudenbush sampler) or `"lmer"` (for [lme4::lmer()]).
#' Version 1.1.1.9000 changed the default to `method = "kr"`.
#'
#' @param control A list with parameters. Use [brokenstick::control_kr()]`,
#' `[lme4::lmerControl()]` or `[brokenstick::set_control()]`.
#'
#' @param seed Seed number for [base::set.seed()]. Use `NA` to bypass
#' seed setting. Only used by method `"kr"`.
#'
#' @param na.action A function that indicates what [lme4::lmer()] should so
#' when the data contain `NA`s. Default set to `na.exclude`. Only used by
#' method `"lmer"`.
#'
#' @param \dots Not currently used, but required for extensibility.
#'
#' @note
#' Note that automatic knot specification is data-dependent, and may not reproduce
#' on other data. Likewise, knots specified via `k` are data-dependent and do not transfer
#' to other  data sets. Fixing the model requires specifying both `knots` and
#' `boundary`.
#'
#' @details
#' The default algorithm since version 2.0 is the Bayesian Kasim-Raudenbush
#' sampler (`method = "kr"`). The variance-covariance matrix of the broken stick
#' estimates absorbs the relations over time. The `"kr"` method allows
#' enforcing a simple structure on this variance-covariance matrix. Currently,
#' there are three such correlation models: `"none"` (default), `"argyle"`
#' and `"cole"`. Specify the `seed` argument for reproducibility.
#' See [kr()] for more details.
#'
#' The alternative is `method = "lmer"`, which fits the broken stick model by
#' [lme4::lmer()]. With this method, the variance-covariance matrix can only be
#' unstructured. This estimate may be unstable if the number of children is
#' small relative to the number of specified knots. The default setting
#' in `[lme4::lmerControl()]` is  `check.nobs.vs.nRE= "stop"`. The
#' `[set_control()]` changes this to `check.nobs.vs.nRE= "warning"`, since
#' otherwise many broken stick models would not run at all. The method
#' throws warnings that estimates are not stable. It can be time
#' consuming for data sets with thousands of children, or for models with
#' many (say > 10) internal knots. Despite the warnings, the results often
#' look reasonable.
#'
#' @return
#'
#' A object of class `brokenstick`.
#'
#' @examples
#' data <- smocc_200[1:1198, ]
#' f1 <- brokenstick(hgt.z ~ age | id, data, knots = 0:3, seed = 1)
#' plot(f1, data, n_plot = 9)
#'
#' # using lmer
#' f2 <- brokenstick(hgt.z ~ age | id, data, knots = 0:3, method = "lmer")
#' plot(f2, data, n_plot = 9)
#'
#' \donttest{
#' # a model with more knots
#' knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24, 36) / 12, 4)
#'
#' # method kr takes about 2 seconds
#' f3 <- brokenstick(hgt.z ~ age | id, data, knots, seed = 2)
#' plot(f3, data, n_plot = 9)
#'
#' # method lmer takes about 40 seconds
#' f4 <- brokenstick(hgt.z ~ age | id, data, knots, method = "lmer")
#' plot(f4, data, n_plot = 9)
#' }
#' @export
brokenstick <- function(formula,
                        data,
                        knots = NULL,
                        boundary = NULL,
                        k = NULL,
                        degree = 1L,
                        method = c("kr", "lmer"),
                        control = set_control(method),
                        seed = NA,
                        na.action = na.exclude,
                        ...) {
  stopifnot(
    inherits(formula, "formula"),
    is.data.frame(data) || is.matrix(data),
    as.integer(degree) %in% c(0L, 1L)
  )
  data <- data.frame(data)
  method <- match.arg(method)
  names <- parse_formula(formula)
  brokenstick_bridge(data, names, knots, boundary, k, degree, method, control,
                     seed, na.action, ...)
}

# ------------------------------------------------------------------------------
# Bridge

brokenstick_bridge <- function(data, names, knots, boundary, k, degree,
                               method, control, seed, na.action,
                               warn_splines = FALSE, ...) {
  y <- data[[names[["y"]]]]
  x <- data[[names[["x"]]]]
  g <- data[[names[["g"]]]]

  stopifnot(
    is.numeric(y),
    is.numeric(x),
    is.numeric(g) || is.factor(g) || is.character(g)
  )

  l <- calculate_knots(x, k, knots, boundary)
  X <- make_basis(x,
    xname = names$x,
    knots = l$knots,
    boundary = l$boundary,
    degree = degree,
    warn = warn_splines
  )

  if (method == "lmer") {
    data_pad <- data.frame(data, X, stringsAsFactors = FALSE)
    names(data_pad) <- c(names(data), colnames(X))
    pred <- paste("0 +", paste(colnames(X), collapse = " + "))
    fm <- as.formula(paste(names$y, "~", pred, "+ (", pred, "|", names$g, ")"))
    fit <- brokenstick_impl_lmer(
      data = data_pad,
      formula = fm,
      control = control,
      na.action = na.action
    )
  }
  if (method == "kr") {
    fit <- kr(
      y = y,
      x = X,
      g = g,
      control = control,
      seed = seed
    )
  }

  new_brokenstick(
    names = names,
    knots = l$knots,
    boundary = l$boundary,
    degree = degree,
    method = method,
    control = control,
    beta = fit$beta,
    omega = fit$omega,
    sigma2j = fit$sigma2j,
    sigma2 = fit$sigma2,
    draws = fit$draws
  )
}


# ------------------------------------------------------------------------------
# Implementation

brokenstick_impl_lmer <- function(data, formula, control, na.action) {

  # Bates et al, linear mixed-effects model
  model <- lmer(
    formula = formula,
    data = data,
    control = control,
    na.action = na.action
  )

  # Here we trust that names(slot(model, "cnms")) gives the name of the
  # group variable
  df <- as.data.frame(VarCorr(model))
  list(
    model = model,
    beta = lme4::fixef(model),
    omega = as.matrix(as.data.frame(VarCorr(model)[[names(slot(model, "cnms"))]])),
    sigma2j = numeric(),
    sigma2 = df[df$grp == "Residual", "vcov"],
    draws = numeric()
  )
}

brokenstick_impl_kr <- function(y, x, g, control, seed) {

  # Kasim-Raudenbush sampler
  kr(
    y = y,
    x = x,
    g = g,
    control = control,
    seed = seed
  )
}
