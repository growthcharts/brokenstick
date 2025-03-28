cran-comments
================

## Submission to CRAN

- brokenstick 2.5.0 was removed from CRAN because of a `chol2inv`
  problem of MKL on Windows. This version fixes the problem by adding a
  more robust version designed to evade the edge cases that caused the
  problem. The package has been tested on Windows, Linux, and macOS.

- Additional minor minor edits:

  - Repaired an URL on FIMD book to evade a redirect.
  - Updated documentation to `royxgen2 7.3.2`

## Test environments

- win-builder: using `devtools::check_win_devel()`
- rhub: `rhub::rc_submit()` with virtual machines: linux, m1-san, macos,
  macos-arm64, windows

## Downstream dependencies

There are no downstream dependencies for this package.
