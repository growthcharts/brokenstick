append_data <- function(data, names, x = NULL, y = NULL, group = NULL) {
  if (is.null(data)) stop("`data` not found.")
  if (is.null(x) && is.null(y) && is.null(group)) {
    return(data)
  }

  # Case 1: create x for every group in data
  if (!is.null(x) && is.null(y) && is.null(group)) {
    reset <- expand.grid(
      x = x,
      y = NA,
      g = unique(data[[names$g]])
    )
    colnames(reset) <- c(names$x, names$y, names$g)
    reset <- bind_rows(data = data, added = reset, .id = ".source")
    # message("Reset newdata: predict at `x` in every group.")
  }

  # Case 2: create new group with x and y
  if (!is.null(x) && !is.null(y) && is.null(group)) {
    if (length(x) != length(y)) {
      stop("Incompatible length of `x` and `y`.", call. = FALSE)
    }
    reset <- data.frame(
      s = "added",
      x = x,
      y = y,
      g = 0
    )
    colnames(reset) <- c(".source", names$x, names$y, names$g)
    # message("Reset newdata: new group with `x` and `y`.")
  }

  # Case 3: subset groups from data
  if (is.null(x) && is.null(y) && !is.null(group)) {
    if (!length(data)) {
      stop("A light brokenstick object expects a `newdata` argument.", call. = FALSE)
    }
    reset <- data[data[[names$g]] %in% group, , drop = FALSE]
    reset <- bind_cols(.source = "data", reset)
    # message("Reset newdata: subset of groups.")
  }

  # Case 4: create x for subset of groups from data
  if (!is.null(x) && is.null(y) && !is.null(group)) {
    groups <- intersect(data[[names$g]], group)
    reset <- expand.grid(
      s = "added",
      x = x,
      y = NA,
      g = groups
    )
    colnames(reset) <- c(".source", names$x, names$y, names$g)
    reset <- bind_rows(
      data = data[data[[names$g]] %in% groups, , drop = FALSE],
      added = reset, .id = ".source"
    ) %>%
      relocate(".source")
    # message("Reset newdata: predict at `x` in subset of groups.")
  }

  # Case 5: create data.frame from vectors x, y and group
  if (!is.null(x) && !is.null(y) && !is.null(group)) {
    if (length(x) != length(y)) {
      stop("Incompatible length of `x` and `y`.", call. = FALSE)
    }
    if (length(x) != length(group)) {
      stop("Incompatible length of `x` and `group`.", call. = FALSE)
    }
    groups <- intersect(data[[names$g]], group)
    reset <- data.frame(
      x = x,
      y = y,
      g = group
    )
    colnames(reset) <- c(names$x, names$y, names$g)
    reset <- bind_rows(
      data = data[data[[names$g]] %in% groups, , drop = FALSE],
      added = reset, .id = ".source"
    ) %>%
      relocate(".source")
    # message("Reset newdata: predict from vectors `x`, `y` and `group`.")
  }

  return(reset)
}

calculate_knots <- function(x, k, internal, boundary) {
  k_orig <- k
  knots_orig <- sort(internal)
  boundary_orig <- sort(boundary)

  # for NULL or length not 2, set boundary to data range
  if (length(boundary_orig) != 2L) {
    boundary <- range(x, na.rm = TRUE)
  }

  # make sure that boundary includes all knots
  if (!is.null(knots_orig)) {
    boundary[1L] <- min(min(knots_orig, na.rm = TRUE), boundary[1L])
    boundary[2L] <- max(max(knots_orig, na.rm = TRUE), boundary[2L])
  }

  if (length(knots_orig)) {
    # if there is vector input via knots
    # trim knots to exclude boundary points, and set k
    knots <- as.numeric(knots_orig)
    knots <- knots[knots > boundary[1L] & knots < boundary[2L]]
    k <- length(knots)
  } else {
    # no user knots, set knots from
    if (is.null(k_orig)) k_orig <- 0L
    if (k_orig >= 0L && k_orig <= 50L) {
      k <- k_orig
      knots <- quantile(x,
        probs = seq(0, 1, length.out = k + 2L),
        na.rm = TRUE
      )[-c(1L, k + 2L)]
    } else {
      stop("Number of internal knots `k` outside range 0-50")
    }
  }
  return(list(k = k, internal = knots, boundary = boundary))
}

install.on.demand <- function(pkg, quiet = FALSE, ...) {
  # internal function that checks whether package pkg is
  # in the library. If not found, it asks the user permission
  # to install from CRAN.
  if (requireNamespace(pkg, quietly = TRUE)) {
    return()
  }
  answer <- askYesNo(paste("Package", pkg, "needed. Install from CRAN?"))
  if (answer) install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = quiet)
  return()
}


rho <- function(t1, t2, tau, lambda) {
  # Argyle et al 2008, eq 2 - Markov model
  return(((tau + t1) / (tau + t2))^lambda)
}

fn <- function(x, data) {
  tau <- x[1L]
  lambda <- x[2L]
  rhat <- with(data, rho(t1, t2, tau, lambda))
  return(sum((data$r - rhat)^2))
}

cor2cov <- function(cor, sd) {
  return(sweep(sweep(cor, 1L, sd, "*"), 2L, sd, "*"))
}

vec2cov <- function(vec, sd) {
  cov <- matrix(0, length(sd), length(sd))
  diag(cov) <- 0.5
  cov[lower.tri(cov)] <- vec
  cov <- cov + t(cov)
  return(cor2cov(cov, sd = sd))
}

cov2vec <- function(cov) {
  cor <- cov2cor(cov)
  return(list(vec = cor[lower.tri(cor)], sd = sqrt(diag(cov))))
}

smooth_covariance <- function(grid, cov, method = c("none", "argyle", "cole")) {
  if (method == "none") {
    return(cov)
  }
  d <- cov2vec(cov)
  grid$r <- d$vec

  # Argyle-Markov correlation model
  if (method == "argyle") {
    opt <- optim(c(1.3, 0.5), fn, data = grid)
    rhat <- with(grid, rho(t1, t2, tau = opt$par[1], lambda = opt$par[2]))
  }

  # Cole correlation model
  if (method == "cole") {
    grid$y <- log((1 + grid$r) / (1 - grid$r)) / 2
    fit <- lm(y ~ I(log((t1 + t2) / 2)) + I(log(t2 - t1)) + I(1 / (t2 - t1)) + I(log((t1 + t2) / 2) * log(t2 - t1)) + I(log((t1 + t2) / 2)^2),
      data = grid
    )
    yhat <- predict(fit)
    rhat <- (exp(2 * yhat) - 1) / (exp(2 * yhat) + 1)
  }

  return(vec2cov(rhat, sd = smooth(d$sd)))
}


#' Parse formula for brokenstick model
#'
#' A bare bones formula parser to extract variables names from
#' formulas of `y ~ x | g`. It return the name of
#' the first variable mentioned in each formula component.
#' @param f formula object
#' @return A `list` with elements `x`, `y` and `g`.
#' Each element has length 1.
#' @author Stef van Buuren 2023
parse_formula <- function(f) {
  stopifnot(inherits(f, "formula"))
  if (length(f[[3L]]) != 3L) {
    stop(call. = FALSE, "Can't find RHS expansion in formula.")
  }
  if (f[[3L]][[1L]] != "|") {
    stop(call. = FALSE, "Can't find `|` operator in formula.")
  }

  # Just take first variables - no support for `+` and friends
  y_name <- all.vars(f[[2L]], max.names = 1L)
  x_name <- all.vars(f[[3L]][[2L]], max.names = 1L)
  g_name <- all.vars(f[[3L]][[3L]], max.names = 1L)

  vec <- c(x_name, y_name, g_name)
  if (any(duplicated(vec))) {
    stop(call. = FALSE, "Found duplicate names in formula.")
  }
  if (any(vec == ".")) {
    stop(call. = FALSE, "No support for `.` in formula.")
  }

  return(list(x = x_name, y = y_name, g = g_name))
}

#' Create linear splines basis
#'
#' This function creates the basis function of a second-order (linear) splines
#' at a user-specific set of break points.
#' @aliases make_basis
#' @param x numeric vector
#' @param xname predictor name. Default is \code{"x"}
#' @param internal a vector of internal knots, excluding boundary knots
#' @param boundary vector of external knots
#' @param degree the degree of the spline. The broken stick model
#' requires linear splines, so the default is \code{degree = 1}.
#' Setting \code{degree = 0} yields (crisp) dummy coding, and one
#' column less than for \code{degree = 1}.
#' @param warn a logical indicating whether warnings from \code{splines::bs()}
#' should be given.
#' @return A matrix with \code{length(x)} rows and \code{length(breaks)}
#' columns, with some extra attributes described by \code{bs()}.
#' @author Stef van Buuren 2023
#' @note Before version 0.54, it was standard practice that the \code{knots}
#' array always included \code{boundary[1L]}.
make_basis <- function(x,
                       xname = "x",
                       internal = NULL,
                       boundary = range(x),
                       degree = 1L,
                       warn = TRUE) {

  # safety check: remove lower boundary knot from knots to be compatiable
  # with models fitted prior to version 0.53
  internal <- internal[internal > boundary[1L] & internal < boundary[2L]]

  # trick to evade error from bs() if x is fully NA
  padx <- all(is.na(x))
  if (padx) x <- c(0, x)

  # dummy coding if degree is zero
  if (degree == 0L) {
    df <- data.frame(x = cut(x,
      breaks = c(boundary[1L], internal, boundary[2L]),
      right = FALSE, include.lowest = TRUE
    ))
    X <- model.matrix(
      as.formula("~ 0 + x"),
      model.frame(~., df, na.action = na.pass)
    )
  }

  # fuzzy coding by linear spline
  if (degree >= 1L) {
    if (warn) {
      X <- splines::bs(
        x = x,
        knots = c(boundary[1L], internal),
        Boundary.knots = boundary,
        degree = degree
      )
    } else {
      suppressWarnings({
        X <- splines::bs(
          x = x,
          knots = c(boundary[1L], internal),
          Boundary.knots = boundary,
          degree = degree
        )
      })
    }
  }

  knots <- sort(unique(c(boundary, internal)))
  if (degree == 0L) knots <- knots[-length(knots)]
  colnames(X) <- paste(xname, as.character(knots), sep = "_")

  # restore original if padded
  if (padx) X <- X[-1L, , drop = FALSE]

  return(X)
}
