library(brokenstick)

fit_lm <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25))

control <- control_brokenstick(method = "kr", kr = list(runin = 100L, ndraw = 10L, par_skip = 10L))
fit_100_10_10 <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)

control <- control_brokenstick(method = "kr", kr = list(runin = 500L, ndraw = 50L, par_skip = 10L))
fit_500_50_10 <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)

control <- control_brokenstick(method = "kr", kr = list(runin = 500L, ndraw = 100L, par_skip = 10L))
fit_500_100_10 <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)

control <- control_brokenstick(method = "kr", kr = list(runin = 500L, ndraw = 1000L, par_skip = 10L))
fit_500_1000_10 <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)

control <- control_brokenstick(method = "kr", kr = list(runin = 5000L, ndraw = 1000L, par_skip = 20L))
fit_inf <- brokenstick(hgt_z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)

