
library(brokenstick)
context("predict()")

exp <- export(fit_hgt)
test_that("returns proper number of rows", {
  expect_equal(nrow(predict(exp, x = NA)), 1)
  expect_equal(nrow(predict(exp, x = NA, y = 10)), 1)
  expect_equal(nrow(predict(exp, x = c(NA, NA), y = c(-1, 10))), 2)
  expect_equal(nrow(predict(exp, x = NA, at = "knots")), 11)
})

meanmodel <- predict(exp)
test_that("returns mean model", {
  expect_equal(predict(exp, x = NA, at = "knots"), meanmodel)
  expect_equal(predict(exp, x = c(NA, NA), at = "knots"), meanmodel)
})

 test_that("accepts intermediate NA in x", {
   expect_equal(
     unlist(predict(exp, x = 1, y = -1)[1, ]),
     unlist(predict(exp, x = c(NA, 1), y = c(1, -1))[2, ]))
   expect_equal(
     unlist(predict(exp, x = c(1, NA), y = c(-1, 1))[1, ]),
     unlist(predict(exp, x = c(NA, 1), y = c(1, -1))[2, ]))
   expect_equal(
     unlist(predict(exp, x = c(1, 2, NA), y = c(NA, -1, 1))[2, ]),
     unlist(predict(exp, x = c(1, NA, 2), y = c(NA,  1, -1))[3, ]))
})

 test_that("accepts unordered x", {
   expect_equal(
     unlist(predict(exp, x = c(1, 2, 3), y = c(-1, 1, 0))[3, ]),
     unlist(predict(exp, x = c(2, 3, 1), y = c(1, 0, -1))[2, ]))
 })