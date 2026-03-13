# Extract Variance and Correlation Components

Extracts variance-covariance or correlation matrix from a `brokenstick`
object.

## Usage

``` r
get_omega(
  x,
  hide = c("right", "left", "boundary", "internal", "none"),
  cor = FALSE,
  whatknots = "all",
  what = "cov"
)
```

## Arguments

- x:

  Object of class `brokenstick`

- hide:

  Should output for knots be hidden in get, print, summary and plot
  functions? Can be `"left"`, `"right"`, `"boundary"`, `"internal"` or
  `"none"`. The default is `"right"`.

- cor:

  Logical. Should the function return the correlation matrix instead of
  the covariance matrix? The default is `FALSE`.

- whatknots:

  Deprecated.

- what:

  Deprecated.

## Value

A numeric matrix, possibly with zero rows and columns if no names match

## Examples

``` r
f1 <- brokenstick(hgt_z ~ age | id, smocc_200[1:1000, ], knots = 0:2, seed = 1)
get_omega(f1, cor = TRUE, hide = "boundary")
#>           age_1     age_2
#> age_1 1.0000000 0.9194458
#> age_2 0.9194458 1.0000000
```
