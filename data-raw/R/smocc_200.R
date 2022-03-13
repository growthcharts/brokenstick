library("donorloader") # local package with data
library("brokenstick")
library("AGD")

# get data
data <- donorloader::load_data(dnr = "smocc", ids = 10001:11120)
smocc_200 <- data$time[, c(
  "id", "age", "sex", "ga", "bw",
  "hgt", "hgt_z"
)]

# The stored smocc$hgt_z uses the preterm references for Z-score calculation.
# Here we use the AGD package, which does not support the preterm references.
# For consistency, we overwrite hgt_z as calculated by the AGD package
hgt_z <- with(
  smocc_200,
  AGD::y2z(
    y = hgt,
    x = age,
    sex = ifelse(sex == "male", "M", "F"),
    ref = nl4.hgt
  )
)
smocc_200$hgt_z <- hgt_z

# fit the brokenstick model
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24) / 12, 4)
fit_200 <- brokenstick(hgt_z ~ age | id,
  data = smocc_200,
  knots = knots, boundary = c(0, 3), seed = 1
)
fit_200_light <- brokenstick(hgt_z ~ age | id,
  data = smocc_200,
  knots = knots, boundary = c(0, 3),
  light = TRUE, seed = 1
)
usethis::use_data(smocc_200, overwrite = TRUE)
usethis::use_data(fit_200, overwrite = TRUE)
usethis::use_data(fit_200_light, overwrite = TRUE)
