# Create linear splines basis

This function creates the basis function of a second-order (linear)
splines at a user-specific set of break points.

## Usage

``` r
make_basis(
  x,
  xname = "x",
  internal = NULL,
  boundary = range(x),
  degree = 1L,
  warn = TRUE
)
```

## Arguments

- x:

  numeric vector

- xname:

  predictor name. Default is `"x"`

- internal:

  a vector of internal knots, excluding boundary knots

- boundary:

  vector of external knots

- degree:

  the degree of the spline. The broken stick model requires linear
  splines, so the default is `degree = 1`. Setting `degree = 0` yields
  (crisp) dummy coding, and one column less than for `degree = 1`.

- warn:

  a logical indicating whether warnings from
  [`splines::bs()`](https://rdrr.io/r/splines/bs.html) should be given.

## Value

A matrix with `length(x)` rows and `length(breaks)` columns, with some
extra attributes described by `bs()`.

## Note

Before version 0.54, it was standard practice that the `knots` array
always included `boundary[1L]`.

## Author

Stef van Buuren 2023
