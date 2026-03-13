# Print brokenstick object

Print brokenstick object

## Usage

``` r
# S3 method for class 'brokenstick'
print(
  x,
  digits = getOption("digits"),
  ...,
  hide = c("right", "left", "boundary", "internal", "none")
)
```

## Arguments

- x:

  A `brokenstick` object

- digits:

  minimal number of *significant* digits, see
  [`print.default`](https://rdrr.io/r/base/print.default.html).

- ...:

  further arguments passed to or from other methods.

- hide:

  Should output for boundary knots be hidden in the print, summary and
  plot functions? Can be `"right"`, `"left"`,`"boundary"`, `"internal"`
  or `"none"`. If not specified, it is read from the field `x$hide`.
