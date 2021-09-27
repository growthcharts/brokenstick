knots <- seq(0, 2, by = 0.25)
boundary <- c(0, 3)

# low n - method kr
# FIXME: Fix, or turn into more informative messages
test_that("brokenstick handles low number of cases", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:10, ],
                           knots = knots, boundary = boundary,
                           method = "kr", seed = 1L),
               "invalid arguments")
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:95, ],
                           knots = knots, boundary = boundary,
                           method = "kr", seed = 2L),
               "invalid arguments")
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:100, ],
                           knots = knots, boundary = boundary,
                           method = "kr", seed = 3L),
               "the leading minor of order ")
  }
)

# # low n - method lmer
# # FIXME: Turn into more informative messages
test_that("brokenstick handles low number of cases", {
  expect_error(brokenstick(hgt.z ~ age | id, smocc_200[1:10, ],
                             knots = knots, boundary = boundary),
                 regexp = "grouping factors must have > 1 sampled level",
                 fixed = TRUE)
  # commented out because this also throws warnings. Checkers don't like that.
  # expect_message(brokenstick(hgt.z ~ age | id, smocc_200[1:11, ],
  #                            knots = knots, boundary = boundary),
  #                regexp = "fixed-effect model matrix is rank deficient",
  #                fixed = TRUE)
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200[1:11, ],
                             knots = knots, boundary = boundary),
                 regexp = "number of observations (=11) <= number of random effects (=20)",
                 fixed = TRUE)
  # commented out because this does not reproduce on the rhub() Lunix flavours
  # expect_message(brokenstick(hgt.z ~ age | id, smocc_200[1:30, ],
  #                            knots = knots, boundary = boundary),
  #                regexp = "boundary (singular) fit: see ?isSingular",
  #                fixed = TRUE)
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200[1:30, ],
                             knots = knots, boundary = boundary),
                 regexp = "number of observations (=30) <= number of random effects (=40)",
                 fixed = TRUE)
  expect_silent(brokenstick(hgt.z ~ age | id, smocc_200[1:200, ],
                             boundary = c(0, 3)))
  }
)

test_that("warns if right boundary is lower than max(age)", {
  expect_warning(brokenstick(hgt.z ~ age | id, smocc_200,
                           knots = 1:2, boundary = c(0, 1),
                           warn_splines = TRUE),
               regexp = "some 'x' values beyond boundary knots may cause ill-conditioned bases",
               fixed = TRUE)
})

