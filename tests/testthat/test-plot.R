context("plot.brokenstick")

# ctl <- control_brokenstick(kr = control_kr(imp_skip = 10))
# knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
# data <- brokenstick:::reset_data(smocc_200[!is.na(smocc_200$hgt.z), ],
#                                  names = list(x = "age", y = "hgt.z", g = "id"),
#                                  x = knots)
# data$.source <- NULL
# fit_kr <- brokenstick(hgt.z ~ age | id, data = data,
#                       knots = knots, boundary = c(0, 3),
#                       method = "kr", control = ctl, seed = 81204)
# plot(fit_kr, new_data = data, show = c(TRUE, FALSE, TRUE), group = c(10001, 10005, 10022))
