# Calculate fitted values

Calculate fitted values

## Usage

``` r
# S3 method for class 'brokenstick'
fitted(object, newdata = NULL, ...)
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

A numerical vector with predictions. The number of elements equals the
number of rows in `newdata`. If `newdata` is not specified, the function
looks for the training data in `object` as the element named `data`.

## See also

Other brokenstick:
[`residuals.brokenstick()`](https://growthcharts.org/brokenstick/reference/residuals.brokenstick.md)
