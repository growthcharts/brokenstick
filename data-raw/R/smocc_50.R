library(donorloader)
library(brokenstick)

# get data
data <- load_data(dnr = "smocc", ids = 10001:10062)
smocc_50 <- data$time[, c("id", "age", "sex", "hgt", "hgt.z")]
colnames(smocc_50)[1] <- "subjid"

# fit the brokenstick model (takes about 20 seconds)
fit_50 <- with(
  smocc_50,
  brokenstick(
    y = hgt.z,
    x = age,
    subjid = subjid,
    knots = round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24) / 12, 4),
    boundary = c(0, 3)
  )
)

usethis::use_data(smocc_50, overwrite = TRUE)
usethis::use_data(fit_50, overwrite = TRUE)
