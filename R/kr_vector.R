## author: Stef van Buuren, 2020

kr_vector <- function(y, ry, x, type, wy = NULL, intercept = TRUE,
                      control) {
  symridge <- function(x, ridge = 0.0001, ...) {
    x <- (x + t(x)) / 2
    if (nrow(x) == 1L) {
      return(x)
    }
    x + diag(diag(x) * ridge)
  }

  ## hack to get knots, assumes that g is last
  xnames <- colnames(x)[-ncol(x)]
  kn <- as.numeric(sub(".*[_]", "", xnames))

  # structure for var-cov cormodel
  grid <- expand.grid(t2 = kn, t1 = kn)
  grid <- data.frame(grid[grid$t1 < grid$t2, ], r = NA)

  ## append intercept
  if (intercept) {
    x <- cbind(1, as.matrix(x))
    type <- c(2, type)
  }

  ## Initialize
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

  store_this_imp <- store_this_draw <- rep(FALSE, control$end)
  if (control$krn) {
    store_this_draw[control$start + (1L:control$krn) * control$thin] <- TRUE
  }
  if (control$krm) {
    store_this_imp[control$start + (1L:control$krm) * control$thin_imp] <- TRUE
  }

  store_beta <- matrix(NA, nrow = control$krn, ncol = n.rc)
  colnames(store_beta) <- xnames

  pnames <- outer(kn, kn, paste, sep = "_")
  pnames <- t(pnames)[lower.tri(t(pnames), diag = TRUE)]
  store_omega <- matrix(NA, nrow = control$krn, ncol = length(pnames))
  colnames(store_omega) <- pnames

  store_sigma2j <- matrix(NA, nrow = control$krn, ncol = n.class)
  colnames(store_sigma2j) <- unique(x[, type == -2])

  store_sigma2 <- matrix(NA, nrow = control$krn, ncol = 1L)

  store_imputes <- matrix(NA, nrow = control$krm, ncol = sum(wy))
  if (control$krm) row.names(store_imputes) <- as.character(1:control$krm)

  count_par <- count_imp <- 0L

  ## Execute Gibbs sampler
  for (iter in seq_len(control$end)) {
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
    psi <- crossprod(t(t(bees) - mu))
    psi_smoothed <- smooth_covariance(grid, psi, method = control$cormodel)

    # Draw psi
    # Add ridge to prevent inversion error with semi-definite psi_smoothed
    # inv.psi <- rWishart(
    #  n = 1, df = n.class - n.rc - 1,
    #  Sigma = chol2inv(chol.default(psi_smoothed + ridge))
    # )[, , 1L]
    nu <- max(n.class - n.rc - 1L, 1L) # prevent negative df
    inv.psi <- matrixsampling::rwishart(
      n = 1L, nu = nu,
      Sigma = chol2inv(chol.default(symridge(psi_smoothed)))
    )[, , 1L]

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
                 scale = 2 / (n.class * (sigma2.0 / H - log(sigma2.0) + log(G) - 1))
    )
    rg <- max(rg, 0.00001) # Prevent extreme
    theta <- 1 / rg

    # Save draws
    if (store_this_draw[iter]) {
      count_par <- count_par + 1L
      store_beta[count_par, ] <- mu
      cov <- chol2inv(chol.default(symridge(inv.psi)))
      store_omega[count_par, ] <- cov[lower.tri(cov, diag = TRUE)]
      store_sigma2j[count_par, ] <- 1 / inv.sigma2
      store_sigma2[count_par, ] <- mean(store_sigma2j[count_par, ])
    }

    if (store_this_imp[iter]) {
      count_imp <- count_imp + 1L
      imputes <- rnorm(n = sum(wy), sd = sqrt(1 / inv.sigma2[gf.full[wy]])) +
        rowSums(as.matrix(x[wy, type == 2, drop = FALSE]) * bees[gf.full[wy], ])
      store_imputes[count_imp, ] <- imputes
    }
  }

  # convert to coda::mcmc
  mcmc <- list(
    beta = mcmc(store_beta, start = control$start, thin = control$thin),
    omega = mcmc(store_omega, start = control$start, thin = control$thin),
    sigma2 = mcmc(store_sigma2, start = control$start, thin = control$thin),
    sigma2j = mcmc(store_sigma2j, start = control$start, thin = control$thin)
  )

  # post-process estimates
  beta <- colMeans(store_beta)
  omega <- colMeans(store_omega)
  sigma2j <- colMeans(store_sigma2j)
  sigma2 <- colMeans(store_sigma2)

  obj <- list(
    y = y,
    ry = ry,
    x = x,
    type = type,
    wy = wy,
    intercept = intercept,
    wy = wy,
    n.class = n.class,
    bees = bees,
    mu = mu,
    inv.psi = inv.psi,
    inv.sigma2 = inv.sigma2,
    sigma2.0 = sigma2.0,
    theta = theta,
    imputes = t(store_imputes),
    beta = beta,
    omega = omega,
    sigma2j = sigma2j,
    sigma2 = sigma2,
    mcmc = mcmc
  )
  class(obj) <- "kr_vector"
  obj
}
