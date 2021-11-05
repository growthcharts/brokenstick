cran-comments
================

## New version

This is a major update of the package.

## Test environments

-   local OS X install, 11.6, R 4.1.1
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
check_rhub()
```

`check_rhub()` produces two notes:

    > checking CRAN incoming feasibility ... NOTE
      
      Size of tarball: 9171284 bytes
      Maintainer: 'Stef van Buuren <stef.vanbuuren@tno.nl>'

    > checking examples ... NOTE
      Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
      predict      7.5   0.00    7.50
      brokenstick  6.2   0.05    6.25

Comment:

The size of the tarball is about 9Mb. This is primarily due to one
extensive vignette html (2.8Mb) that includes lots of plots. This
vignette is a useful resource for the end user. I therefore suggest that
the CRAN maintainers accept the package as is, but please let me know of
any alternative options to reduce package size. Also, I would ask the
CRAN maintainer to see through the slightly above threshold CPU time, as
both example sections demonstrate many capabilities of these functions
to the end user.

## Downstream dependencies

There are no downstream dependencies for this package.
