cran-comments
================

## Resubmission to CRAN

-   This submission reduces package size from 9.2Mb to 3.7Mb by taking
    out temporary files that should not be in the distribution

## Test environments

-   local OS X install, 11.6, R 4.1.2
-   win-builder, using `devtools::check_win_devel()`
-   rhub

### Local build

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK brokenstick_2.0.0.tar.gz

Status: OK
```

### WIN_DEVEL

`devtools::check_win_devel()` resulted in:

    Status: OK

### RHUB

``` r
rhub::check_for_cran()
```

`rhub::check_for_cran()` produces “SUCCESS” (three times) and one note:

    N  checking examples
       Examples with CPU (user + system) or elapsed time > 5s
                   user system elapsed
       predict     7.83   0.02    7.84
       brokenstick 6.06   0.02    6.08

Comment: Both example sections demonstrate many capabilities of these
functions to the end user. I would ask the CRAN maintainer to see
through the slightly above threshold CPU time.

## Downstream dependencies

There are no downstream dependencies for this package.
