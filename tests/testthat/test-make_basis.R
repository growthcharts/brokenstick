context("make_basis")

d0 <- make_basis(data.frame(weight = women$height), knots = c(58, 64, 68, 72), degree = 0)
d1 <- make_basis(data.frame(weight = women$height), knots = c(58, 64, 68, 72), degree = 1)

test_that("make_basis work with degree = 0 and 1", {
  expect_equal(ncol(d0), 3L)
  expect_equal(ncol(d1), 4L)
})
