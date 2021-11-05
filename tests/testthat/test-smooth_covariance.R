grid <- expand.grid(
  t2 = c(0, 6, 13, 26, 39, 52, 78),
  t1 = c(0, 6, 13, 26, 39, 52, 78)
)
grid <- grid[grid$t1 < grid$t2, ]
grid <- data.frame(grid, r = c(
  0.724, 0.605, 0.487, 0.452, 0.439, 0.442,
  0.885, 0.734, 0.644, 0.622, 0.578,
  0.863, 0.779, 0.742, 0.672,
  0.909, 0.865, 0.775,
  0.935, 0.849,
  0.898
))

# argyle model
opt <- optim(c(1.3, 0.5), brokenstick:::fn, data = grid)
rhat <- with(grid, brokenstick:::rho(t1, t2, tau = opt$par[1], lambda = opt$par[2]))
cov_argyle <- brokenstick:::vec2cov(rhat, sd = rep(1, 7))

# cole model
cov <- brokenstick:::vec2cov(grid$r, sd = rep(1, 7))
cov_cole <- brokenstick:::smooth_covariance(grid, cov, "cole")
