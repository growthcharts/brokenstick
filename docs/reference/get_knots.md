# Obtain the knots from a broken stick model

Obtain the knots from a broken stick model

## Usage

``` r
get_knots(
  object,
  hide = c("right", "left", "boundary", "internal", "none"),
  whatknots = "all",
  what = "all"
)
```

## Arguments

- object:

  An object of class `brokenstick`

- hide:

  Should output for knots be hidden in get, print, summary and plot
  functions? Can be `"left"`, `"right"`, `"boundary"`, `"internal"` or
  `"none"`. The default is `"right"`.

- whatknots:

  Deprecated. Use `hide` instead.

- what:

  Deprecated. Use `hide` instead.

## Value

A vector with knot locations, either both, internal only or boundary
only, depending on `hide`. The result is `NULL` if `object` does not
have proper class. Returns `numeric(0)` if there are no internal knots.

## Examples

``` r
get_knots(fit_200, hide = "bo")
#> [1] 0.0833 0.1667 0.2500 0.5000 0.7500 1.0000 1.2500 1.5000 2.0000
```
