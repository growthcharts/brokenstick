# Check perfect model

## Objective

In general, the broken stick model smoothes the observed growth
trajectory. What happens of all observations are already aligned to the
break ages? Does the model perfectly represent the data? Is the
covariance matrix of the random effects ($`\Omega)`$ equal to the
covariance between the measurements? Is $`\sigma^2`$ equal to zero?

## Data generation

We adapt code from
<http://www.davekleinschmidt.com/sst-mixed-effects-simulation/simulations_slides.pdf>
to generate test data:

``` r
library("plyr")
library("mvtnorm")
make_data_generator <- function(resid_var = 1,
                                ranef_covar = diag(c(1, 1)), n = 100
                                ) {
  ni <- nrow(ranef_covar)
  generate_data <- function() {
    # sample data set under mixed effects model with random slope/intercepts 
    simulated_data <- rdply(n, {
      b <- t(rmvnorm(n = 1, sigma = ranef_covar))
      epsilon <- rnorm(n = length(b), mean = 0, sd = sqrt(resid_var))
      b + epsilon
    })
  data.frame(
    subject = rep(1:n, each = ni),
    age = rep(1:ni, n),
    simulated_data)
  }
}
```

We choose between the perfect situation where $`\sigma^2 = 0`$ and the
noisy case $`\sigma^2 = 1`$ and where the ages align perfectly.

``` r
resid_var <- 0
resid_var <- 1
set.seed(77711)
covar <- matrix(c(1, 0.7, 0.5, 0.3,
                  0.7, 1, 0.8, 0.5,
                  0.5, 0.8, 1, 0.6,
                  0.3, 0.5, 0.6, 1), nrow = 4)
gen_dat <- make_data_generator(n = 10000, 
                               ranef_covar = covar,
                               resid_var = resid_var)
data <- gen_dat()
head(data)
```

    ##   subject age .n      X1
    ## 1       1   1  1 -0.9478
    ## 2       1   2  1 -2.0837
    ## 3       1   3  1 -2.6512
    ## 4       1   4  1 -2.5526
    ## 5       2   1  2 -0.0825
    ## 6       2   2  2 -1.2707

We wish to reproduce the correlation matrix among the $`y`$’s from the
mixed model estimates. The **target correlation matrix** is:

``` r
library("tidyr")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
d <- as_tibble(data[,-3])
broad <- t(spread(d, subject, X1))[-1,]
cor(broad)
```

    ##       [,1]  [,2]  [,3]  [,4]
    ## [1,] 1.000 0.350 0.255 0.161
    ## [2,] 0.350 1.000 0.406 0.246
    ## [3,] 0.255 0.406 1.000 0.313
    ## [4,] 0.161 0.246 0.313 1.000

## Fit model

Fit broken stick model, with knots specified at ages `1:4`.

``` r
library("brokenstick")
knots <- 1:3
boundary <- c(1, 4)
fit <- brokenstick(X1 ~ age | subject, data, 
                   knots = knots, boundary = boundary,
                   method = "lmer")
```

    ## Warning: number of observations (=40000) <= number of random effects (=40000)
    ## for term (0 + age_1 + age_2 + age_3 + age_4 | subject); the random-effects
    ## parameters and the residual variance (or scale parameter) are probably
    ## unidentifiable

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 1 negative eigenvalues

``` r
omega <- get_omega(fit, hide = "no")
beta <- coef(fit, hide = "no")
sigma2 <- fit$sigma2
round(beta, 2)
```

    ## age_1 age_2 age_3 age_4 
    ## -0.02 -0.02  0.01  0.01

``` r
round(sigma2, 4)
```

    ## [1] 0.818

``` r
# correlation random effects
round(covar, 3)
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]  1.0  0.7  0.5  0.3
    ## [2,]  0.7  1.0  0.8  0.5
    ## [3,]  0.5  0.8  1.0  0.6
    ## [4,]  0.3  0.5  0.6  1.0

``` r
round(omega, 2)
```

    ##       age_1 age_2 age_3 age_4
    ## age_1  1.23  0.71  0.52  0.33
    ## age_2  0.71  1.16  0.82  0.49
    ## age_3  0.52  0.82  1.22  0.63
    ## age_4  0.33  0.49  0.63  1.17

``` r
# covariances measured data
round(omega + diag(sigma2, 4), 3)
```

    ##       age_1 age_2 age_3 age_4
    ## age_1 2.052 0.706 0.521 0.326
    ## age_2 0.706 1.982 0.816 0.489
    ## age_3 0.521 0.816 2.034 0.630
    ## age_4 0.326 0.489 0.630 1.992

``` r
round(cov(broad), 3)
```

    ##       [,1]  [,2]  [,3]  [,4]
    ## [1,] 2.052 0.706 0.521 0.326
    ## [2,] 0.706 1.982 0.816 0.489
    ## [3,] 0.521 0.816 2.034 0.630
    ## [4,] 0.326 0.489 0.630 1.992

``` r
# convert to time-to-time correlation matrix
round(cov2cor(omega + diag(sigma2, 4)), 3)
```

    ##       age_1 age_2 age_3 age_4
    ## age_1 1.000 0.350 0.255 0.161
    ## age_2 0.350 1.000 0.406 0.246
    ## age_3 0.255 0.406 1.000 0.313
    ## age_4 0.161 0.246 0.313 1.000

``` r
round(cor(broad), 3)
```

    ##       [,1]  [,2]  [,3]  [,4]
    ## [1,] 1.000 0.350 0.255 0.161
    ## [2,] 0.350 1.000 0.406 0.246
    ## [3,] 0.255 0.406 1.000 0.313
    ## [4,] 0.161 0.246 0.313 1.000

``` r
z <- predict(fit, x = "knots", include_data = FALSE, shape = "wide")[, -1]
# off-diagonal elements of covariance of broken stick estimates approach correlation
# not enough variance in the diagonal because of smoothing
cov(z)
```

    ##       1     2     3
    ## 1 0.795 0.595 0.475
    ## 2 0.595 0.791 0.681
    ## 3 0.475 0.681 0.821

``` r
# correlations of broken stick estimates are inflated because of smoothing
cor(z)
```

    ##       1     2     3
    ## 1 1.000 0.750 0.587
    ## 2 0.750 1.000 0.845
    ## 3 0.587 0.845 1.000

## Conclusions

1.  If $`\sigma^2=0`$, then the off-diagonal elements of $`\Omega`$
    reproduce the correlations among the $`y`$’s. The estimate of
    $`\sigma^2`$ is too high (about 0.13 instead of 0).
2.  If $`\sigma^2 > 0`$, then $`\hat C = \Omega + \hat\sigma^2 I(n_i)`$
    reproduces the sample covariance matrix between $`y`$’s exactly.
3.  `cov2cor(hatC)` reproduces the sample time-to-time correlation
    matrix.
4.  Conclusion: The unbiased estimate of the time-to-time correlation
    matrix among the (unobserved) measurements at the knots:

``` r
cov <- get_omega(fit)
chat <- cov + diag(fit$sigma2, nrow(cov))
r <- cov2cor(chat)
r
```

    ##       age_1 age_2 age_3
    ## age_1 1.000 0.350 0.255
    ## age_2 0.350 1.000 0.406
    ## age_3 0.255 0.406 1.000
