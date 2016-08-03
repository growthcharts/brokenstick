
library(brokenstick)
context("Arguments of predict.brokenstick.export()")

exp <- export.brokenstick(fit.hgt)
meanmodel <- predict(exp)

test_that("predict.brokenstick.export() returns mean model", {
  expect_equal(predict(exp), meanmodel)
  expect_equal(predict(exp, x = NA), meanmodel)
  expect_equal(predict(exp, x = NA, y = 10), meanmodel)
  expect_equal(predict(exp, x = c(NA, NA), y = c(10, 10)), meanmodel)
})

test_that("predict.brokenstick.export() steps over missing x", {
expect_equal(predict(exp, x = c(1, NA), y = c(10, 10)),
               meanmodel)
  expect_equal(predict(exp, x = c(NA, 1), y = c(10, 10)),
               meanmodel)
  expect_equal(predict(exp, x = c(NA, 1, 2), y = c(NA, NA, NA)),
               meanmodel)
})
