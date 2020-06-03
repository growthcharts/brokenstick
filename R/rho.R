
rho <- function(t1, t2, tau, lambda) {
  # Argyle et al 2008, eq 2 - Markov model
  ((tau + t1) / (tau + t2))^lambda
}

fn <- function(x, data) {
  tau <- x[1L]
  lambda <- x[2L]
  rhat <- with(data, rho(t1, t2, tau, lambda))
  sum((data$r - rhat)^2)
}

cor2cov <- function(cor, sd) {
  sweep(sweep(cor, 1L, sd, "*"), 2L, sd, "*")
}

vec2cov <- function(vec, sd) {
  cov <- matrix(0, length(sd), length(sd))
  diag(cov) <- 0.5
  cov[lower.tri(cov)] <- vec
  cov <- cov + t(cov)
  cor2cov(cov, sd = sd)
}

cov2vec <- function(cov) {
  cor <- cov2cor(cov)
  list(vec = cor[lower.tri(cor)], sd = sqrt(diag(cov)))
}

smooth_covariance <- function(grid, cov, method = c("none", "argyle", "cole")) {
  if (method == "none") return(cov)
  d <- cov2vec(cov)
  grid$r <- d$vec

  # Argyle-Markov correlation model
  if (method == "argyle") {
  opt <- optim(c(1.3, 0.5), fn, data = grid)
  rhat <- with(grid, rho(t1, t2, tau = opt$par[1], lambda = opt$par[2]))
  }

  # Cole correlation model
  if (method == "cole") {
    grid$y <- log((1 + grid$r)/(1 - grid$r)) / 2
    fit <- lm(y ~ I(log((t1+t2)/2)) + I(log(t2-t1)) + I(1/(t2-t1)) + I(log((t1+t2)/2)*log(t2-t1)) + I(log((t1+t2)/2)^2),
              data = grid)
    yhat <- predict(fit)
    rhat <- (exp(2 * yhat) - 1) / (exp(2 * yhat) + 1)
  }

  vec2cov(rhat, sd = smooth(d$sd))
}
