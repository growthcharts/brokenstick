# Set controls for Kasim-Raudenbush sampler

Set controls for Kasim-Raudenbush sampler

## Usage

``` r
control_kr(
  niter = 200L,
  nimp = 0L,
  start = 101L,
  thin = 1L,
  seed = NA_integer_,
  cormodel = c("none", "argyle", "cole"),
  ...
)
```

## Arguments

- niter:

  Integer. Number of samples from posterior. Default: `200`.

- nimp:

  Integer. Number of multiple imputations. Default: `0`.

- start:

  Integer. The iteration number of the first observation

- thin:

  Integer. The thinning interval between consecutive observations

- seed:

  Integer. Seed number for
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html). Use `NA` to
  bypass seed setting.

- cormodel:

  String indicating the correlation model: `"none"` (default),
  `"argyle"` or `"cole"`

- ...:

  Allow for dot parameters

## Value

A list with eight components. The function calculates parameters `end`
(the iteration number of the last iteration) and `thin_imp` (thinning
factor for multiple imputations) from the other inputs.
