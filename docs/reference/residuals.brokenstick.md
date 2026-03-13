# Extract residuals from brokenstick model

Extract residuals from brokenstick model

## Usage

``` r
# S3 method for class 'brokenstick'
residuals(object, newdata = NULL, ...)
```

## Arguments

- object:

  A `brokenstick` object.

- newdata:

  Optional. A data frame in which to look for variables with which to
  predict. The training data are used if omitted and if `object$light`
  is `FALSE`.

- ...:

  Additional arguments. Ignored.

## Value

A numerical vector with residuals The number of elements equals the
number of rows in `newdata`. If `newdata` is not specified, the function
looks for the training data in `object` as the element named `data`.

## See also

Other brokenstick:
[`fitted.brokenstick()`](https://growthcharts.org/brokenstick/reference/fitted.brokenstick.md)
