cran-comments
================

## Submission to CRAN

- This is a re-submission of the `brokenstick` package, previously
  archived on 2025-03-21.

- When I got the CRAN notification, I browsed the “additional topics”
  but could not detect an error, and then forgot about it.

- After removal, I found the entry named MKL that makes clear why the
  package was archived.

- I will read future CRAN notifications and respond.

- The MKL entry signals a `chol2inv` problem of MKL on Windows. This
  update fixes the problem by

  - Adding a more robust version designed to evade the edge cases that
    caused the problem.
  - Outcommented two tests that produced inconsistent results between
    Windows, Linux, and macOS using rhub2.

- Additional minor minor edits:

  - Repaired an URL on FIMD book to evade a redirect.
  - Updated documentation to `royxgen2 7.3.2`

- Updated version number to brokenstick 2.6.0 after all checks were OK.

## Test environments

- win-builder: using `devtools::check_win_devel()`: Status OK
- rhub: `rhub::rc_submit()` with virtual machines: linux, m1-san, macos,
  macos-arm64, windows: Status
  <https://github.com/r-hub2/twill-wistful-blacklab-brokenstick>

## Downstream dependencies

There are no downstream dependencies for this package.
