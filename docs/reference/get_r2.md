# Obtain proportion of explained variance from a broken stick model

Obtain proportion of explained variance from a broken stick model

## Usage

``` r
get_r2(object, newdata = NULL)
```

## Arguments

- object:

  An object of class `brokenstick`

- newdata:

  Data on which `r.squared` must be calculated

## Value

Proportion of explained variance

## Examples

``` r
get_r2(fit_200)
#> [1] 0.9808152
get_r2(fit_200_light, newdata = smocc_200)
#> [1] 0.9808152
```
