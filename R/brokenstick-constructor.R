new_brokenstick <- function(names = list(x = character(),
                                         y = character(),
                                         z = character()),
                            knots = numeric(0),
                            boundary = numeric(0),
                            degree = 1L,
                            model = NULL,
                            method = NA_character_,
                            control = list(),
                            beta = numeric(0),
                            omega = numeric(0),
                            sigma2j = numeric(0),
                            sigma2 = numeric(0),
                            draws = numeric(0)) {
  result <- list(names = names,
                 knots = knots,
                 boundary = boundary,
                 degree = degree,
                 model = model,
                 method = method,
                 control = control,
                 beta = beta,
                 omega = omega,
                 sigma2j = sigma2j,
                 sigma2 = sigma2,
                 draws = draws)
  class(result) <- "brokenstick"
  result
}
