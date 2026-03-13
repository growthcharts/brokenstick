# Broken stick model with nine lines for 200 children (light)

Object `fit_200_light` has class `brokenstick` and stores the the model
settings and parameter estimates.

## Format

An object of class
[brokenstick](https://growthcharts.org/brokenstick/reference/brokenstick-class.md),
fitted by the
[`brokenstick()`](https://growthcharts.org/brokenstick/reference/brokenstick.md).

## Details

The datasets was constructed as

    knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
    fit_200_light <- brokenstick(hgt_z ~ age | id, data = smocc_200,
                           knots = knots,
                           light = TRUE, seed = 1)
