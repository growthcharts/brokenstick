library(brokenstick)
context("predict_new.brokenstick()")

exp <- fit_200
dat <- smocc_200
test_that("returns proper number of rows", {
  expect_equal(nrow(predict_new(exp, dat, x = NA)), 200L)
  expect_equal(nrow(predict_new(exp, dat, x = c(NA, NA))), 400L)
  expect_equal(nrow(predict_new(exp, dat, x = NA, y = 1)), 1L)
  expect_equal(nrow(predict_new(exp, dat, x = c(NA, NA), y = c(-1, 10))), 2L)
  expect_equal(nrow(predict_new(exp, dat, x = "knots")), 2200L)
  expect_equal(nrow(predict_new(exp, dat, x = "knots", y = rep(1, 11))), 11L)
})

test_that("accepts intermediate NA in x", {
  expect_equal(
    unlist(predict_new(exp, x = 1, y = -1)[1, ]),
    unlist(predict_new(exp, x = c(NA, 1), y = c(1, -1))[2, ])
  )
  expect_equal(
    unlist(predict_new(exp, x = c(1, NA), y = c(-1, 1))[1, ]),
    unlist(predict_new(exp, x = c(NA, 1), y = c(1, -1))[2, ])
  )
  expect_equal(
    unlist(predict_new(exp, x = c(1, 2, NA), y = c(NA, -1, 1))[2, ]),
    unlist(predict_new(exp, x = c(1, NA, 2), y = c(NA, 1, -1))[3, ])
  )
})

test_that("accepts unordered x", {
  expect_equal(
    round(predict_new(exp, x = c(1, 2, 3), y = c(-1, 1, 0))[1, 5], 5),
    round(predict_new(exp, x = c(2, 3, 1), y = c(1, 0, -1))[3, 5], 5)
  )
})
