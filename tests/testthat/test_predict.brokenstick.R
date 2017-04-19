
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
  expect_equal(nrow(predict(obj, x = NA, at = "both")), n + m)
  expect_equal(nrow(predict(obj, x = NA, y = 10, at = "both")), 1 + k)
  expect_equal(nrow(predict(obj, x = c(NA, NA), y = c(-1, 10), at = "both")), 2 + k)
})

test_that("output = 'vector' and output = 'long' are consistent", {
  expect_equivalent(predict(obj)$yhat,
               predict(obj, output = "vector"))
  expect_equal(predict(obj, x = NA)$yhat,
               predict(obj, x = NA, output = "vector"))
  expect_equal(predict(obj, x = c(NA, 1), y = c(1, NA))$yhat,
               predict(obj, x = c(NA, 1), y = c(1, NA), 10, output = "vector"))
  expect_equal(predict(obj, at = "knots")$yhat,
               predict(obj, at = "knots", output = "vector"))
  expect_equal(predict(obj, x = NA, at = "knots")$yhat,
               predict(obj, x = NA, at = "knots", output = "vector"))
  expect_equal(predict(obj, x = NA, y = 10, at = "knots")$yhat,
               predict(obj, x = NA, y = 10, at = "knots", output = "vector"))
  expect_equal(predict(obj, at = "both")$yhat,
               predict(obj, at = "both", output = "vector"))
  expect_equal(predict(obj, x = NA, at = "both")$yhat,
               predict(obj, x = NA, at = "both", output = "vector"))
  expect_equal(predict(obj, x = NA, y = 10, at = "both")$yhat,
               predict(obj, x = NA, y = 10, at = "both", output = "vector"))
})


