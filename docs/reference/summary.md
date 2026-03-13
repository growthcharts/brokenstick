# Create summary of brokenstick object

Create summary of brokenstick object

## Usage

``` r
# S3 method for class 'brokenstick'
summary(
  object,
  ...,
  cor = FALSE,
  lower = TRUE,
  hide = c("right", "left", "boundary", "internal", "none")
)
```

## Arguments

- object:

  A `brokenstick` object

- ...:

  additional arguments affecting the summary produced.

- cor:

  Logical. Should the function return the correlation matrix instead of
  the covariance matrix? The default is `FALSE`.

- lower:

  Logical. Print lower triangle of correlation/covariance matrix?

- hide:

  Should output for boundary knots be hidden in the print, summary and
  plot functions? Can be `"left"`, `"right"`, `"boundary"`, `"internal"`
  or `"none"`. If not specified, it is read from the field
  `object$hide`.
