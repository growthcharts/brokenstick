
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
  list(vec = cor[upper.tri(cor)], sd = sqrt(diag(cov)))
}

smooth_covariance <- function(grid, cov) {
  # apply Argyle-Markov correlation model
  d <- cov2vec(cov)
  grid$r <- d$vec
  opt <- optim(c(1.3, 0.5), fn, data = grid)
  rhat <- with(grid, rho(t1, t2, tau = opt$par[1], lambda = opt$par[2]))
  vec2cov(rhat, sd = smooth(d$sd))
}

# data <- expand.grid(t2 = c(0, 6, 13, 26, 39, 52, 78),
#                     t1 = c(0, 6, 13, 26, 39, 52, 78))
# data <- data[data$t1 < data$t2, ]
# data <- data.frame(data, r = c(0.724, 0.605, 0.487, 0.452, 0.439, 0.442,
#                                0.885, 0.734, 0.644, 0.622, 0.578,
#                                0.863, 0.779, 0.742, 0.672,
#                                0.909, 0.865, 0.775,
#                                0.935, 0.849,
#                                0.898))
#
# opt <- optim(c(1.3, 0.5), fn, data = data)
# rhat <- with(data, rho(t1, t2, tau = opt$par[1], lambda = opt$par[2]))
# vec2cov(rhat, sd = rep(1, 7))

