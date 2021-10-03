knots <- seq(0, 2, by = 0.25)
boundary <- c(0, 3)

# low n - method kr - EDGE cases
test_that("brokenstick handles low number of cases - 1", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:10, ],
                           knots = knots, boundary = boundary,
                           method = "kr", seed = 1L),
               "`Sigma` is symmetric but not positive")
})

test_that("brokenstick handles low number of cases - 2", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:95, ],
                           knots = knots, boundary = boundary,
                           method = "kr", seed = 2L),
               "`Sigma` is symmetric but not positive")
})

test_that("brokenstick handles low number of cases - 3", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:100, ],
                           knots = knots, boundary = boundary,
                           method = "kr", seed = 3L),
               "`Sigma` is symmetric but not positive")
})

test_that("brokenstick handles low number of cases - 4", {
  expect_silent(brokenstick(hgt.z ~ age | id, smocc_200[1:200, ],
                            knots = knots, boundary = boundary,
                            method = "kr", seed = 4L))
})


# # low n - method lmer - EDGE cases
test_that("brokenstick handles low number of cases - 1", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:10, ],
                           knots = knots, boundary = boundary,
                           method = "lmer"),
               regexp = "grouping factors must have > 1 sampled level",
               fixed = TRUE)
})

test_that("brokenstick handles low number of cases - 2", {
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200[1:11, ],
                             knots = knots, boundary = boundary,
                             method = "lmer"),
                 regexp = "number of observations (=11) <= number of random effects (=20)",
                 fixed = TRUE)
})

test_that("brokenstick handles low number of cases - 3", {
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200[1:30, ],
                             knots = knots, boundary = boundary,
                             method = "lmer"),
                 regexp = "number of observations (=30) <= number of random effects (=40)",
                 fixed = TRUE)
})

test_that("brokenstick handles low number of cases - 4", {
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200[1:200, ],
                            knots = knots, boundary = boundary,
                            method = "lmer"),
                 regexp = "number of observations (=199) <= number of random effects (=210)",
                 fixed = TRUE)
  }
)

test_that("warns that right boundary is lower than max(age)", {
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200,
                             knots = 1:2, boundary = c(0, 1),
                             warn_splines = TRUE),
                 regexp = "some 'x' values beyond boundary knots may cause ill-conditioned bases",
                 fixed = TRUE)
})

