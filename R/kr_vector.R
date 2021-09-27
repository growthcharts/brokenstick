## author: Stef van Buuren, 2020

kr_vector <- function(y, ry, x, type, wy = NULL, intercept = TRUE,
                      model = "none",
                      runin = 100L, ndraw = 10L, par_skip = 10L,
                      imp_skip = Inf) {

  symridge <- function(x, ridge = 0.0001, ...) {
    x <- (x + t(x))/2
    if (nrow(x) == 1L) return(x)
    x + diag(diag(x) * ridge)
  }

  ## hack to get knots, assumes that g is last
  kn <- as.numeric(sub(".*[_]", "", colnames(x)[-ncol(x)]))

  # structure for var-cov model
  grid <- expand.grid(t2 = kn, t1 = kn)
  grid <- data.frame(grid[grid$t1 < grid$t2, ], r = NA)

  ## append intercept
  if (intercept) {
    x <- cbind(1, as.matrix(x))
    type <- c(2, type)
  }

  ## Initialize
  n.iter <- runin + ndraw * par_skip
  draw <- c(rep(FALSE, runin), rep(c(rep(FALSE, par_skip - 1L), TRUE), ndraw))

  nimp <- floor(ndraw / imp_skip)
  if (nimp)
    impute <- c(rep(FALSE, runin),
                rep(c(rep(FALSE, min(imp_skip, ndraw) - 1), TRUE), nimp))
  else
    impute <- rep(FALSE, n.iter)

  if (is.null(wy)) wy <- !ry
  n.class <- length(unique(x[, type == -2]))
  if (n.class == 0) stop("No class variable")
  gf.full <- factor(x[, type == -2], labels = seq_len(n.class))
  gf <- gf.full[ry]
  XG <- split.data.frame(as.matrix(x[ry, type == 2]), gf)
  X.SS <- lapply(XG, crossprod)
  yg <- split(as.vector(y[ry]), gf)
  n.g <- tabulate(gf)
  n.rc <- ncol(XG[[1]])

  bees <- matrix(0, nrow = n.class, ncol = n.rc)
  ss <- vector(mode = "numeric", length = n.class)
  mu <- rep.int(0, n.rc)
  inv.psi <- diag(1, n.rc, n.rc)
  ridge <- diag(0.0001, n.rc, n.rc)
  inv.sigma2 <- rep.int(1, n.class)
  sigma2.0 <- 1
  theta <- 1

  store_beta <- matrix(NA, nrow = ndraw, ncol = n.rc)
  store_omega <- array(NA, c(ndraw, n.rc, n.rc))
  store_sigma2j <- matrix(NA, nrow = ndraw, ncol = n.class)
  store_sigma2 <- matrix(NA, nrow = ndraw, ncol = 1L)
  store_draws <- matrix(NA, nrow = nimp, ncol = sum(wy))
  count_par <- count_imp <- 0L

  ## Execute Gibbs sampler
  for (iter in seq_len(n.iter)) {
    ## Draw bees
    for (class in seq_len(n.class)) {
      vv <- inv.sigma2[class] * X.SS[[class]] + inv.psi
      bees.var <- chol2inv(chol.default(symridge(vv)))
      bees[class, ] <- drop(bees.var %*% (crossprod(inv.sigma2[class] * XG[[class]], yg[[class]]) + inv.psi %*% mu)) +
        drop(rnorm(n = n.rc) %*% chol.default(bees.var))
      ss[class] <- crossprod(yg[[class]] - XG[[class]] %*% bees[class, ])
    }

    # Draw mu
    mu <- colMeans(bees) + drop(rnorm(n = n.rc) %*% chol.default(chol2inv(chol.default(symridge(inv.psi))) / n.class))

    # Enforce simple structure on psi
    method <- ifelse(n.iter <= runin, model, model)
    psi <- crossprod(t(t(bees) - mu))
    psi_smoothed <- smooth_covariance(grid, psi, method = method)

    # Draw psi
    # Add ridge to prevent inversion error with semi-definite psi_smoothed
    #inv.psi <- rWishart(
    #  n = 1, df = n.class - n.rc - 1,
    #  Sigma = chol2inv(chol.default(psi_smoothed + ridge))
    #)[, , 1L]
    nu <- max(n.class - n.rc - 1L, 1L)  # prevent negative df
    inv.psi <- matrixsampling::rwishart(
      n = 1L, nu = nu,
      Sigma = chol2inv(chol.default(symridge(psi_smoothed))))[, , 1L]

    ## Draw sigma2
    shape <- n.g / 2 + 1 / (2 * theta)
    inv.sigma2 <- rgamma(n.class, shape = shape, scale = 2 * theta / (ss * theta + sigma2.0))

    ## Draw sigma2.0
    H <- 1 / mean(inv.sigma2) # Harmonic mean
    sigma2.0 <- rgamma(1, n.class / (2 * theta) + 1, scale = 2 * theta * H / n.class)

    ## Draw theta
    G <- exp(mean(log(1 / inv.sigma2))) # Geometric mean
    shape <- max(n.class / 2 - 1, 0.01) # Prevent negative shape
    rg <- rgamma(1,
                 shape = shape,
                 scale = 2 / (n.class * (sigma2.0 / H - log(sigma2.0) + log(G) - 1)))
    rg <- max(rg, 0.00001) # Prevent extreme
    theta <- 1 / rg

    # Save draws
    if (draw[iter]) {
      count_par <- count_par + 1L
      store_beta[count_par, ] <- mu
      store_omega[count_par, , ] <- chol2inv(chol.default(symridge(inv.psi)))
      store_sigma2j[count_par, ] <- 1/inv.sigma2
      store_sigma2[count_par, ] <- mean(store_sigma2j[count_par, ])
    }

    if (impute[iter]) {
      count_imp <- count_imp + 1L
      imps <- rnorm(n = sum(wy), sd = sqrt(1 / inv.sigma2[gf.full[wy]])) +
        rowSums(as.matrix(x[wy, type == 2, drop = FALSE]) * bees[gf.full[wy], ])
      store_draws[count_imp, ] <- imps
    }
  }

  # post-process estimates
  beta <- colMeans(store_beta)
  omega <- apply(store_omega, c(2L, 3L), mean)
  sigma2j <- colMeans(store_sigma2j)
  sigma2 <- colMeans(store_sigma2)
  draws <- t(store_draws)

  obj <- list(
    y = y,
    ry = ry,
    x = x,
    type = type,
    wy = wy,
    intercept = intercept,
    n.iter = n.iter,
    wy = wy,
    n.class = n.class,
    bees = bees,
    mu = mu,
    inv.psi = inv.psi,
    inv.sigma2 = inv.sigma2,
    sigma2.0 = sigma2.0,
    theta = theta,
    imps = draws,
    beta = beta,
    omega = omega,
    sigma2j = sigma2j,
    sigma2 = sigma2,
    draws = draws
  )
  class(obj) <- "kr_vector"
  obj
}
