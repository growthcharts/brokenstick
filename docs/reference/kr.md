# Kasim-Raudenbush sampler for two-level normal model

Simulates posterior distributions of parameters from a two-level normal
model with heterogeneous within-cluster variances (Kasim and Raudenbush,
1998). Imputations can be drawn as an extra step to the algorithm.

## Usage

``` r
kr(y, x, g, control = control_kr())
```

## Arguments

- y:

  Vector with outcome value

- x:

  Matrix with predictor value

- g:

  Vector with group values

- control:

  A list created by
  [`control_kr()`](https://growthcharts.org/brokenstick/reference/control_kr.md)
  that sets algorithmic options of the sampler and correlation model.

## Value

An object of class `kr`, basically a list with components:

    * `beta`  Fixed effects
    * `omega` Variance-covariance of random effects
    * `sigma2_j` Residual variance per group
    * `sigma2` Average residual variance
    * `sample` Descriptive statistics about the data
    * `imp`   Numeric matrix with `nimp` multiple imputations.
    * `mod`   A list of objects of class [coda::mcmc()]

The number of rows in `imp` is equal to the number of missing values in
the outcome vector `y`. The number of columns equals `nimp`.

## Details

The speed of the Kasim-Raudenbush sampler is almost independent of the
number of random effect, and foremost depends on the total number of
iterations.

The defaults `start = 100`, `n = 200` and `thin = 1` provide 200
parameter draws with a *reasonable* approximation to the
variance-covariance matrix of the random effects.

For a closer approximations with 200 draws set
`control = control_kr(thin = 10)` (*better*) or `thin = 20` (*best*), at
the expense of a linear increase in calculation time. Drawing fewer than
50 observations is not recommended, and such results are best treated as
*indicative*.

It is possible to draw multiple imputations by setting the `nimp`
parameter. For example, to draw five imputations for each missing
outcome specify `control = control_kr(nimp = 5)`.

## References

Kasim RM, Raudenbush SW. (1998). Application of Gibbs sampling to nested
variance components models with heterogeneous within-group variance.
Journal of Educational and Behavioral Statistics, 23(2), 93–116.

## Author

Stef van Buuren, based on
[`mice::mice.impute.2l.norm()`](https://amices.org/mice/reference/mice.impute.2l.norm.html)
