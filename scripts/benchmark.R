library(microbenchmark)
library(brokenstick)

lm1 <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = 0:3), times = 10)
lm2 <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.5)), times = 10)
lm3 <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25)), times = 10)
fit_lm <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25))

control <- control_brokenstick(method = "kr")
kr1 <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = 0:3, control = control), times = 10)
kr2 <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.5), control = control), times = 10)
kr3 <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control), times = 10)

fit_kr4_simridge <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.1), control = control)
fit_kr4_no_simridge <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.1), control = control)

kr4_no_symridge <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.1), control = control), times = 10)
kr4_yes_symridge <- microbenchmark(brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.1), control = control), times = 10)
