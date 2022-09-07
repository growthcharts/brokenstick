cran-comments
================

## Submission to CRAN

-   `brokenstick 2.3.0` incorporates various updates and fixes. See
    NEWS.md for an overview.
-   The version also updates the roxygen2 version

## Test environments

-   local OS X, 12.5.1, R 4.2.1
-   win-builder, using `devtools::check_win_devel()`
-   rhub

### Local build

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK brokenstick_2.3.0.tar.gz
Status: OK
```

### WIN_DEVEL

`devtools::check_win_devel()` resulted in:

    OK

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
