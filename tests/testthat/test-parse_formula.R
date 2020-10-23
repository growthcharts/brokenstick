context("parse_formula")

# allowed syntax
f1 <- y ~ x | z
f2 <- y + y1 ~ x + x1 | z + z1
f3 <- y + y1 + log(y2) ~ x + x1 * x2 | z + z1
f4 <- y ~ x + y | z

target <- list(x = "x", y = "y", g = "z")

test_that("operates normally", {
  expect_identical(parse_formula(f1), target)
  expect_identical(parse_formula(f2), target)
  expect_identical(parse_formula(f3), target)
  expect_identical(parse_formula(f4), target)
})

# invoke errors
f5 <- y ~ x + x1
f6 <- y ~ . + a | z
f7 <- y ~ y | z
f8 <- y ~ x

test_that("throws error", {
  expect_error(parse_formula(f5))
  expect_error(parse_formula(f6))
  expect_error(parse_formula(f7))
  expect_error(parse_formula(f8))
})

