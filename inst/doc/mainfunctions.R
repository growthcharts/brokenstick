## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 3.5)

## ----message=FALSE-------------------------------------------------------
if (!require(rbokeh)) devtools::install_github("hafen/rbokeh")
if (!require(hbgd)) devtools::install_github("HBGDki/hbgd")
require("hbgd")
require("brokenstick")

## ------------------------------------------------------------------------
smc <- brokenstick::smocc_hgtwgt
head(smc, 3)

## ----plotmeasurements----------------------------------------------------
fit <- brokenstick(y = smc$htcm, x = smc$age, subjid = smc$subjid)
ids <- c(10001, 10005, 10022)
plot(fit, ids = ids, zband = FALSE, height = 350, width = 225, ylab = "Length (cm)")

## ----bokeh---------------------------------------------------------------
fit0 <- brokenstick(y = smc$haz, x = smc$age, subjid = smc$subjid)
plot(fit0, ids = ids, width = 225, height = 350)

## ----ggplot, fig.height=3, fig.width=7, warning=FALSE--------------------
plot(fit0, ids = ids, pkg = "ggplot")

## ----fit2, cache = TRUE--------------------------------------------------
fit2 <- brokenstick(y = smc$haz, x = smc$age, subjid = smc$subjid, knots = 1:2)

## ----plot_fit1-----------------------------------------------------------
plot(fit2, ids = ids, x_trim = c(0, 2.2), width = 225, height = 350)

## ----fit9, cache = TRUE, warning=FALSE-----------------------------------
# 10 scheduled visits
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
fit9 <- brokenstick(y = smc$haz, x = smc$age, subjid = smc$subjid, knots = knots)

## ------------------------------------------------------------------------
plot(fit9, ids = ids, x_trim = c(0, 2.2), width = 225, height = 350)

## ------------------------------------------------------------------------
p1 <- predict(fit2)
head(p1, 4)

## ------------------------------------------------------------------------
p2 <- predict(fit2, at = "knots")
head(p2, 4)

## ------------------------------------------------------------------------
get_pev(fit2)

## ------------------------------------------------------------------------
get_pev(fit9)

## ------------------------------------------------------------------------
subj <- get_subject_data(smc)
head(subj, 3)

## ------------------------------------------------------------------------
bs <- predict(fit9, at = "knots", output = "broad")
dim(bs)
head(round(bs, 2), 3)

## ------------------------------------------------------------------------
data <- cbind(subj, bs)
fit1_lm <- lm(`2` ~ sex + gagebrth + I(birthwt / 1000), data = data)
summary(fit1_lm)

## ------------------------------------------------------------------------
fit2_lm <- lm(`2` ~ sex + gagebrth + I(birthwt / 1000) + `0`, data = data)
summary(fit2_lm)

