
library(brokenstick)
context("predict.brokenstick()")

obj <- fit_206
data <- get_xy(obj)
n <- nrow(data)
m <- length(unique(data$subjid))
k <- length(get_knots(obj))

test_that("returns proper number of rows", {
  expect_equal(nrow(predict(obj)), n)
  expect_equal(nrow(predict(obj, x = NA)), n + m)
  expect_equal(nrow(predict(obj, x = NA, y = 10)), 1)
  expect_equal(nrow(predict(obj, x = c(NA, NA), y = c(-1, 10))), 2)
})

test_that("returns proper number of rows with at = 'knots'", {
  expect_equal(nrow(predict(obj, at = "knots")), m * k)
  expect_equal(nrow(predict(obj, x = NA, at = "knots")), m + n)
  expect_equal(nrow(predict(obj, x = NA, y = 10, at = "knots")), k)
  expect_equal(nrow(predict(obj, x = c(NA, NA), y = c(-1, 10), at = "knots")), k)
})

test_that("returns proper number of rows with at = 'both'", {
  expect_equal(nrow(predict(obj, at = "both")), n + k * m)
  # expect_equal(nrow(predict(obj, x = NA, at = "both")), n + k * m + m)
  expect_equal(nrow(predict(obj, x = NA, y = 10, at = "both")), 1 + k)
  expect_equal(nrow(predict(obj, x = c(NA, NA), y = c(-1, 10), at = "both")), 2 + k)
})


