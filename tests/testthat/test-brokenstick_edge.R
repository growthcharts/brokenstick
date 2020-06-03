context("brokenstick-edge")

knots <- seq(0, 2, by = 0.25)
boundary <- c(0, 3)
control <- control_brokenstick(method = "kr")

# low n - method kr
# FIXME: Fix, or turn into more informative messages
test_that("brokenstick handles low number of cases", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:10, ],
                           knots = knots, boundary = boundary,
                           control = control, seed = 1L),
               "invalid arguments")
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:95, ],
                           knots = knots, boundary = boundary,
                           control = control, seed = 2L),
               "invalid arguments")
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:100, ],
                           knots = knots, boundary = boundary,
                           control = control, seed = 3L),
               "the leading minor of order ")
  }
)

control <- control_brokenstick(method = "lmer")
# # low n - method lmer
# # FIXME: Turn into more informative messages
test_that("brokenstick handles low number of cases", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:10, ],
                             knots = knots, boundary = boundary,
                             control = control),
                 regexp = "grouping factors must have > 1 sampled level",
                 fixed = TRUE)
  expect_message(brokenstick(hgt.z ~ age | id, smocc_200[1:11, ],
                             knots = knots, boundary = boundary,
                             control = control),
                 regexp = "fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients",
                 fixed = TRUE)
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200[1:11, ],
                             knots = knots, boundary = boundary,
                             control = control),
                 regexp = "number of observations (=11) <= number of random effects (=20)",
                 fixed = TRUE)
  expect_message(brokenstick(hgt.z ~ age | id, smocc_200[1:30, ],
                             knots = knots, boundary = boundary,
                             control = control),
                 regexp = "boundary (singular) fit: see ?isSingular",
                 fixed = TRUE)
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200[1:30, ],
                             knots = knots, boundary = boundary,
                             control = control),
                 regexp = "number of observations (=30) <= number of random effects (=40)",
                 fixed = TRUE)
  expect_silent(brokenstick(hgt.z ~ age | id, smocc_200[1:200, ],
                             boundary = c(0, 3)))
  }
)

