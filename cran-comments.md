cran-comments
================

## Submission to CRAN

-   `brokenstick 2.4.0` incorporates many updates and fixes for
    JSS-manuscript version 30-10-2002. See NEWS.md for an overview.

## Test environments

-   local: OS X, 12.6, R 4.2.1, aarch64-apple-darwin20 (64-bit)
-   win-builder: using `devtools::check_win_devel()`
-   rhub: `rhub::check_for_cran()`

### Local build

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK brokenstick_2.4.0.tar.gz
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

    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'

I believe this note is not related to the brokenstick package.

## Downstream dependencies

There are no downstream dependencies for this package.
