library(brokenstick)
set.seed(1)

control <- control_brokenstick(method = "kr", kr = list(runin = 5000L, ndraw = 1000L, par_skip = 20L))
fit_inf <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)

fit_dif <- function(f1, f2) {
  om1 <- f1$omega[-nrow(f1$omega), -nrow(f1$omega)]
  om2 <- f2$omega[-nrow(f2$omega), -nrow(f2$omega)]
  sum((om1 - om2)^2)
}

testcondition <- function(runin = 100, ndraw = 10, par_skip = 10, replic = 100,
                          truth = fit_inf, seed = 1) {
  set.seed(seed)
  control <- control_brokenstick(method = "kr", kr = list(runin = runin, ndraw = ndraw, par_skip = par_skip))
  result <- rep(NA, replic)
  for (i in 1:replic) {
    fit <- brokenstick(hgt.z ~ age | id, data = smocc_200, knots = seq(0, 2, 0.25), control = control)
    result[i] <- fit_dif(fit, truth)
  }
  median(result)
}

go <- function(replic = 20, truth = fit_inf, seed = 1) {
  runin <- c(100)
  ndraw <- c(50, 100, 200)
  par_skip <- c(1, 10, 20)

  scenarios <- expand.grid(runin = runin, ndraw = ndraw, par_skip = par_skip)
  result <- rep(NA, nrow(scenarios))
  for (j in 1:nrow(scenarios)) {
    cat("Condition: ", j, "\n")
    result[j] <- testcondition(runin = scenarios[j, "runin"],
                            ndraw = scenarios[j, "ndraw"],
                            par_skip = scenarios[j, "par_skip"],
                            replic = replic,
                            seed = j)
  }
  data.frame(
    scenarios,
    result = round(result, 3L))
}

res <- go(replic = 20)

write.table(res, file = "simulate_1_result.txt",
            quote = FALSE, sep = "\t", row.names = FALSE)

