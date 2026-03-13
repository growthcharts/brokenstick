# Empirical Bayes predictor for random effects

This function can estimate random effect for a given set of model
estimates and new user data. The unit may be new to the model. The
methods implements the EB estimate (also known as BLUP) as described in
Skrondral and Rabe-Hasketh, 2009, p. 683. This function can also provide
the broken stick estimate for a given level, the sum of the global
(fixed) and individual (random) effects. The current implementation does
not provide prediction errors.

## Usage

``` r
EB(model, y, X, Z = X, BS = TRUE)
```

## Arguments

- model:

  An object of class `brokenstick`.

- y:

  A vector of new measurements for unit j, scaled in the same metric as
  the fitted model.

- X:

  A `nj * p` matrix with fixed effects for unit j, typically produced by
  `bs()`.

- Z:

  A `nj * q` matrix with random effects for unit j. The default sets `Z`
  equal to `X`.

- BS:

  A logical indicating whether broken stick estimates should be returned
  (`BS = TRUE`) or the random effects (`BS = FALSE`). The default is
  `TRUE`.

## Value

A vector of length q containing the random effect or broken stick
estimates for unit j.

## References

Skrondal, A., Rabe-Hesketh, S. (2009). Prediction in multilevel
generalized linear models. J. R. Statist. Soc. A, 172, 3, 659-687.

## Author

Stef van Buuren 2023
