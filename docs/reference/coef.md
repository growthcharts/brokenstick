# Extract Model Coefficients from brokenstick Object

Extract Model Coefficients from brokenstick Object

## Usage

``` r
# S3 method for class 'brokenstick'
coef(
  object,
  complete = TRUE,
  ...,
  hide = c("right", "left", "boundary", "internal", "none")
)
```

## Arguments

- object:

  A `brokenstick` object

- complete:

  for the default (used for `lm`, etc) and `aov` methods: logical
  indicating if the full coefficient vector should be returned also in
  case of an over-determined system where some coefficients will be set
  to [`NA`](https://rdrr.io/r/base/NA.html), see also
  [`alias`](https://rdrr.io/r/stats/alias.html). Note that the default
  *differs* for [`lm()`](https://rdrr.io/r/stats/lm.html) and
  [`aov()`](https://rdrr.io/r/stats/aov.html) results.

- ...:

  other arguments.

- hide:

  Should output for boundary knots be hidden in the print, summary and
  plot functions? Can be `"right"`, `"left"`, `"boundary"`, `"internal"`
  or `"none"`. If not specified, it is read from `object$hide`.
