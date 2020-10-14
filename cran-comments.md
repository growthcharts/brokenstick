cran-comments
================

## Reason for submission

`brokenstick 1.0.0` is a new package

## Test environments

  - local OS X install, 10.15.7, R 4.0.2
  - win-builder, using `devtools::check_win_devel()`
  - rhub

### Local build

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK brokenstick_1.0.0.tar.gz

Status: OK
```

### WIN\_DEVEL

`devtools::check_win_devel()` resulted in:

    Status: OK

### RHUB

``` r
check_rhub()
```

The result is:

    Build ID:   brokenstick_0.78.0.tar.gz-61572f5f668243edae78a65a4cb5259d
    Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    Submitted:  7 minutes 11 seconds ago
    Build time: 6 minutes 32.2 seconds
    
    ...
    Error in loadNamespace(name) : there is no package called 'utf8'

The solution of this problem is outside my reach.

    Build ID:   brokenstick_0.78.0.tar.gz-498bf3a054274ed69819b3c32a4d65af
    Platform:   Ubuntu Linux 16.04 LTS, R-release, GCC
    Submitted:  26 minutes 35.1 seconds ago
    Build time: 26 minutes 14.8 seconds

Status: Success. Just a “new submission” note.

    Build ID:   brokenstick_0.78.0.tar.gz-ce06a5c3e53d45f0bdf64595537d0c12
    Platform:   Fedora Linux, R-devel, clang, gfortran
    Submitted:  32 minutes 31.7 seconds ago
    Build time: 31 minutes 52.9 seconds

Status: Success. Just a “new submission” note.

## Downstream dependencies

There are no downstream dependencies for this package.
