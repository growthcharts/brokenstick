library(donorloader)
library(brokenstick)

# sample 50 children
set.seed(13112)
data <- load_data(dnr = "smocc")
ids <- sort(sample(data$child$id, 50))

# get data
data <- load_data(dnr = "smocc", ids = ids)
smocc_50 <- data$time[, c("id", "age", "sex", "hgt", "hgt.z")]

# fit the brokenstick model (takes about 20 seconds)
fit_50 <- with(
  smocc_50,
  brokenstick(
    y = hgt.z,
    x = age,
    subjid = id,
    knots = round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24) / 12, 4),
    boundary = c(0, 3)
  )
)

usethis::use_data(smocc_50, overwrite = TRUE)
usethis::use_data(fit_50, overwrite = TRUE)
