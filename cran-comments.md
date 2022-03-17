cran-comments
================

## Resubmission 3 to CRAN

-   We are now at version `brokenstick 2.1.0`
-   Incorporates changes asked for by J Stat Soft (see package NEWS)
-   Reduces package size from 3.7 Mb to 1.1 Mb by transferring large
    vignette to GitHub site

## Resubmission 2 to CRAN

-   The submission changes a URL classified as 502 “Bad Gateway”
-   The submission wraps examples in `\dontrun` from `predict()` to
    reduce CPU

## Resubmission 1 to CRAN

-   This submission reduces package size from 9.2Mb to 3.7Mb by taking
    out temporary files that should not be in the distribution

## Test environments

-   local OS X install, 12.2.1, R 4.1.3
-   win-builder, using `devtools::check_win_devel()`
-   rhub

### Local build

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK brokenstick_2.1.0.tar.gz

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
