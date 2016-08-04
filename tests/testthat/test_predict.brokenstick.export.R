
library(brokenstick)
context("Arguments of predict.brokenstick.export()")

exp <- export.brokenstick(fit.hgt)
test_that("predict() returns proper number of rows", {
  expect_equal(nrow(predict(exp, x = NA)), 1)
  expect_equal(nrow(predict(exp, x = NA, y = 10)), 1)
  expect_equal(nrow(predict(exp, x = c(NA, NA), y = c(-1, 10))), 2)
  expect_equal(nrow(predict(exp, x = NA, type = "atknots")) , 11)
})

meanmodel <- predict(exp)
test_that("predict() returns mean model", {
  expect_equal(predict(exp, x = NA, type = "atknots"), meanmodel)
})


desired <- predict(exp, x = c(1, NA), y = c(-1, 1))
test_that("predict.brokenstick.export() steps over missing x", {
  expect_equal(predict(exp, x = c(1, NA), y = c(-1, 1)), desired)
  expect_equal(predict(exp, x = c(NA, 1), y = c(-1, 1)), desired)
  expect_equal(predict(exp, x = c(NA, 1, 2), y = c(NA, NA, NA)), desired)
})
