data_orig <- smocc_200[1:1000, c("id", "age", "hgt", "hgt.z")]
data <- data_orig
f <- as.formula("hgt.z ~ age | id")

test_that("silent when data is tibble", {
   expect_silent(brokenstick(f, data, knots = 0:3))
})

data$id <- factor(data$id)
test_that("silent when group is factor", {
  expect_silent(brokenstick(f, data, knots = 0:3))
})

data$id <- as.character(data$id)
test_that("silent when group is character", {
  expect_silent(brokenstick(f, data, knots = 0:3))
})

data$id <- as.character(data$id)
test_that("silent when group is character", {
  expect_silent(brokenstick(f, data, knots = 0:3))
})

data$hgt.z <- as.integer(data$hgt.z)
test_that("accepts integer outcome", {
  expect_silent(brokenstick(f, data, knots = 0:3))
})

data <- data_orig
data$hgt.z <- sample(c(TRUE, FALSE), 1000, replace = TRUE)
test_that("accepts no logical outcome", {
  expect_error(brokenstick(f, data, knots = 0:2))
})

data <- data_orig
data$hgt.z <- cut(data$hgt.z, breaks = c(-2, -1, 0, 1, 2))
test_that("accepts no factor outcome", {
  expect_error(brokenstick(f, data, knots = 0:2))
})

data <- data_orig
data$age <- cut(data$age, breaks = seq(0, 2, 0.5))
test_that("accepts no factor predictor", {
  expect_error(brokenstick(f, data, knots = 0:2))
})

data$age <- sample(c(TRUE, FALSE), 1000, replace = TRUE)
test_that("accepts no logical predictor", {
  expect_error(brokenstick(f, data, knots = 0:2))
})
