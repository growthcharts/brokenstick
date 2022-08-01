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
#' The function sorts the internal knots in increasing order.
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
#' then `knots` takes precedence. The default is `k = 5`, so if the user
#' does not specify any of `knots`, `boundary` or `k`, then the knots
#' will be at the 16th, 33th, 50th, 66th and 84th quantile of the
#' predictor.
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
#' @param control List of control options returned by [set_control()] used
#'    to set algorithmic details. A list with parameters. When not specified,
#'    the functions sets to defaults
#'    for method `"kr"` by [brokenstick::control_kr()], and
#'    for method `"lmer"` by [lme4::lmerControl()]. For ease of use, the user
#'    may set individual options to `"kr"` (e.g. `niter = 500`) via the \dots
#'    arguments.
#'
#' @param na.action A function that indicates what [lme4::lmer()] should so
#' when the data contain `NA`s. Default set to `na.exclude`. Only used by
#' method `"lmer"`.
#'
#' @param light Should the returned object be lighter? If `light = TRUE`
#'    the returned object will contain only the model settings and parameter
#'    estimates and not store the `data`, `imp` and `mod` elements. The light
#'    object can be used to predict broken stick estimates for new data, but
#'    does not disclose the training data and is very small (often <20 Kb).
#'
#' @param \dots Forwards arguments to [brokenstick::control_kr()].
#'
#' @note
#' Note that automatic knot specification is data-dependent, and may not reproduce
#' on other data. Likewise, knots specified via `k` are data-dependent and do not transfer
#' to other  data sets. Fixing the model requires specifying both `knots` and
#' `boundary`.
#'
#' @details
#' The choice between `method = "kr"` and `method = "lmer"` depends on the size
#' of the data and the complexity of the model. In general, setting `method = "lmer"`
#' can require substantial calculation time for more complex models
#' (say > 8 internal knots) and may not converge. Method `"kr"` is less
#' sensitive to model complexity and small samples, and has the added benefit that the
#' variance-covariance matrix of the random effects can be constrained through the
#' `cormodel` argument. On the other hand, `"lmer"` is the better-researched
#' method, and is more efficient for simpler models and datasets with many
#' rows.
#'
#' The default algorithm since version 2.0 is the Bayesian Kasim-Raudenbush
#' sampler (`method = "kr"`). The variance-covariance matrix of the broken stick
#' estimates absorbs the relations over time. The `"kr"` method allows
#' enforcing a simple structure on this variance-covariance matrix. Currently,
#' there are three such correlation models: `"none"` (default), `"argyle"`
#' and `"cole"`. Specify the `seed` argument for reproducibility.
#' See [control_kr()] for more details.
#'
#' The alternative `method = "lmer"` fits the broken stick model by
#' [lme4::lmer()]. With this method, the variance-covariance matrix can only be
#' unstructured. This estimate may be unstable if the number of children is
#' small relative to the number of specified knots. The default setting
#' in [lme4::lmerControl()] is  `check.nobs.vs.nRE= "stop"`. The
#' `[set_control()]` function changes this to `check.nobs.vs.nRE= "warning"`
#' by default, since otherwise many broken stick models would not run at all.
#' The method throws warnings that estimates are not stable. It can be time
#' for models with many internal knots. Despite the warnings,
#' the results often look reasonable.
#'
#' Diagnostics with \pkg{coda} and \pkg{lme4}: The function returns an object
#' of class `brokenstick`. For `method = "kr"` the list component named
#' `"mod"` contains a list of `mcmc` objects that can be further analysed with
#' [coda::acfplot()], [coda::autocorr()], [coda::crosscorr()], [coda::cumuplot()],
#' [coda::densplot()], [coda::effectiveSize()], [coda::geweke.plot()],
#' [coda::raftery.diag()], [coda::traceplot()] and the usual `plot()`
#' and `summary()` functions. For `method = "lmer"` the list component named
#' `"mod"` contains an object of class [lme4::merMod]. These model objects
#' are omitted in light `brokenstick` objects.
#' @return
#'
#' A object of class `brokenstick`.
#'
#' @examples
#' \donttest{
#' data <- smocc_200[1:1198, ]
#'
#' # using kr method, default
#' f1 <- brokenstick(hgt_z ~ age | id, data, knots = 0:2, seed = 123)
#' plot(f1, data, n_plot = 9)
#'
#' # study sampling behaviour of the sigma2 parameter with coda
#' library("coda")
#' plot(f1$mod$sigma2)
#' acfplot(f1$mod$sigma2)
#'
#' # using lmer method
#' f2 <- brokenstick(hgt_z ~ age | id, data, knots = 0:2, method = "lmer")
#' plot(f2, data, n_plot = 9)
#'
#' # drill down into merMod object with standard diagnostics in lme4
#' summary(f2$mod)
#' plot(f2$mod)
#'
#' # a model with more knots
#' knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24, 36) / 12, 4)
#'
#' # method kr takes about 2 seconds
#' f3 <- brokenstick(hgt_z ~ age | id, data, knots, seed = 222)
#' plot(f3, data, n_plot = 9)
#'
#' # method lmer takes about 40 seconds
#' f4 <- brokenstick(hgt_z ~ age | id, data, knots, method = "lmer")
#' plot(f4, data, n_plot = 9)
#' }
#' @export
brokenstick <- function(formula,
                        data,
                        knots = NULL,
                        boundary = NULL,
                        k = 5L,
                        degree = 1L,
                        method = c("kr", "lmer"),
                        control = set_control(method = method, ...),
                        na.action = na.exclude,
                        light = FALSE,
                        ...) {
  call <- match.call()
  stopifnot(
    inherits(formula, "formula"),
    is.data.frame(data) || is.matrix(data),
    as.integer(degree) %in% c(0L, 1L)
  )
  data <- data.frame(data)
  method <- match.arg(method)
  obj <- brokenstick_bridge(
    formula, data, knots, boundary, k, degree,
    method, control, na.action, light, call,
    ...
  )
  return(obj)
}

# ------------------------------------------------------------------------------
# Bridge

brokenstick_bridge <- function(formula, data, knots, boundary, k, degree,
                               method, control, na.action, light, call,
                               warn_splines = FALSE, ...) {
  names <- parse_formula(formula)
  nms <- unname(unlist(names))
  if (!all(nms %in% colnames(data))) {
    stop("Variable(s) not found: ",
      paste(nms[!nms %in% colnames(data)], collapse = ", "),
      call. = FALSE
    )
  }

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
    internal = l$internal,
    boundary = l$boundary,
    degree = degree,
    warn = warn_splines
  )

  if (method == "kr") {
    fit <- kr(
      y = y,
      x = X,
      g = g,
      control = control
    )
  }
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

  obj <- new_brokenstick(
    call = call,
    names = names,
    internal = l$internal,
    boundary = l$boundary,
    degree = degree,
    method = method,
    control = control,
    beta = fit$beta,
    omega = fit$omega,
    sigma2j = fit$sigma2j,
    sigma2 = fit$sigma2,
    light = light,
    data = data,
    sample = fit$sample,
    imp = fit$imp,
    mod = fit$mod
  )
  return(obj)
}

# ------------------------------------------------------------------------------
# Implementation

brokenstick_impl_lmer <- function(data, formula, control, na.action) {

  # Bates et al, linear mixed-effects model
  mod <- lmer(
    formula = formula,
    data = data,
    control = control,
    na.action = na.action
  )

  # Here we trust that names(slot(model, "cnms")) gives the name of the
  # group variable
  df <- as.data.frame(VarCorr(mod))
  y <- mod@resp$y
  obj <- list(
    mod = mod,
    beta = fixef(mod),
    omega = as.matrix(as.data.frame(VarCorr(mod)[[names(slot(mod, "cnms"))]])),
    sigma2j = numeric(),
    sigma2 = df[df$grp == "Residual", "vcov"],
    sample = c(
      length(y), sum(!is.na(y)), sum(is.na(y)), as.integer(ngrps(mod)),
      0L
    )
  )
  return(obj)
}
