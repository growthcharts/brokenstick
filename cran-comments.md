cran-comments
================

## Submission to CRAN

- **The DOI in the CITATION is for a new JSS publication that will be
  registered after publication on CRAN.**
- `brokenstick 2.5.0` updates the package documentation and site to sync
  with the manuscript to appear in JSS in March 2023
- See NEWS.md for an overview of (minor) edits.

## Test environments

- local: OS X, 13.2.1, R 4.2.3, aarch64-apple-darwin20 (64-bit)
- win-builder: using `devtools::check_win_devel()`
- rhub: `rhub::check_for_cran()`

### Local build

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK brokenstick_2.5.0.tar.gz
Status: OK
```

### WIN_DEVEL

`devtools::check_win_devel()` resulted in:

    Status: 1 NOTE

    Found the following (possibly) invalid DOIs:
      DOI: 10.18637/jss.v106.i07
        From: DESCRIPTION
              inst/CITATION
        Status: 404
        Message: Not Found

- The missing DOI is expected. See above.

### RHUB

``` r
rhub::check_for_cran()
```

    Ubuntu Linux 20.04.1 LTS, R-release, GCC: Finished: SUCCESS

    Fedora Linux, R-devel, clang, gfortran: Finished: SUCCESS

    * checking CRAN incoming feasibility ... [8s/35s] NOTE
    Maintainer: ‘Stef van Buuren <stef.vanbuuren@tno.nl>’

    Found the following (possibly) invalid DOIs:
      DOI: 10.18637/jss.v106.i07
        From: DESCRIPTION
              inst/CITATION
        Status: 404
        Message: Not Found

    * checking examples ... [9s/33s] NOTE
    Examples with CPU (user + system) or elapsed time > 5s
               user system elapsed
    get_omega 3.257  0.045  12.458

    * checking HTML version of manual ... NOTE
    Skipping checking HTML validation: no command 'tidy' found

- The missing DOI is expected. See above.
- `elapsed time > 5s`: user + system is below \< 5s
- `no command 'tidy' found`: I don’t know what to do about this.

<!-- -->

    Windows Server 2022, R-devel, 64 bit: Finished: SUCCESS

    * checking CRAN incoming feasibility ... [12s] NOTE
    Maintainer: 'Stef van Buuren <stef.vanbuuren@tno.nl>'

    Found the following (possibly) invalid DOIs:
      DOI: 10.18637/jss.v106.i07
        From: DESCRIPTION
              inst/CITATION
        Status: 404
        Message: Not Found
        
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:

- The missing DOI is expected. See above.
- `detritus` message seems unrelated to `brokenstick` package.

## Downstream dependencies

There are no downstream dependencies for this package.
