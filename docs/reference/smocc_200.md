# Infant growth of 0-2 years, SMOCC data extract

Longitudinal height and weight measurements during ages 0-2 years for a
representative sample of 1933 Dutch children born in 1988-1989. The
dataset `smocc_200` is sample of size 200 from the full data.

## Format

A tibble with 1942 rows and 7 columns:

- id:

  ID, unique `id` of each child (numeric)

- age:

  Decimal age, 0-2.68 years (numeric)

- sex:

  Sex, `"male"` or `"female"` (character)

- ga:

  Gestational age, completed weeks (numeric)

- bw:

  Birth weight in grammes (numeric)

- hgt:

  Height measurement in cm (numeric)

- hgt_z:

  Height in SDS relative Fourth Dutch Growth Study 1997 (numeric)

## Source

Herngreen WP, van Buuren S, van Wieringen JC, Reerink JD,
Verloove-Vanhorick SP & Ruys JH (1994). Growth in length and weight from
birth to 2 years of a representative sample of Netherlands children
(born in 1988-89) related to socio-economic status and other background
characteristics. *Annals of Human Biology*, **21**, 449-463.
