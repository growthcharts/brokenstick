# Parse formula for brokenstick model

A bare bones formula parser to extract variables names from formulas of
`y ~ x | g`. It return the name of the first variable mentioned in each
formula component.

## Usage

``` r
parse_formula(f)
```

## Arguments

- f:

  formula object

## Value

A `list` with elements `x`, `y` and `g`. Each element has length 1.

## Author

Stef van Buuren 2023
