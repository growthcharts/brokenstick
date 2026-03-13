# Robust inversion of symmetric matrices

Attempts to compute the inverse of a symmetric matrix using Cholesky
decomposition. If the matrix is not positive definite, a small ridge
value is added. If it still fails, a diagonal fallback is returned.

## Usage

``` r
robust_chol2inv(Sigma, eps = 1e-08, fallback_diag = TRUE)
```

## Arguments

- Sigma:

  A symmetric matrix to invert.

- eps:

  Ridge value added to the diagonal to regularize the matrix.

- fallback_diag:

  Logical. If `TRUE`, return a diagonal matrix as a fallback when
  Cholesky fails.

## Value

A matrix of the same dimension as `Sigma`, representing its regularized
inverse.
