library(donorloader)
library(brokenstick)

# get data
data <- load_data(dnr = "smocc", ids = 10001:11120)
smocc_200 <- data$time[, c("id", "age", "sex", "ga", "bw" ,
                           "hgt", "hgt.z")]

# fit the brokenstick model (takes about 20 seconds)
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
fit_200 <- brokenstick(hgt.z ~ age | id, data = smocc_200,
                       knots = knots, boundary = c(0, 3))

usethis::use_data(smocc_200, overwrite = TRUE)
usethis::use_data(fit_200, overwrite = TRUE)
