## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 3.5)

## ------------------------------------------------------------------------
if (!require(rbokeh)) install.packages("rbokeh")
if (!require(hbgd)) devtools::install_github("HBGDki/hbgd")
require("brokenstick")

## ------------------------------------------------------------------------
smc <- brokenstick::smocc_hgtwgt[1:2000, ]
head(smc, 3)

## ------------------------------------------------------------------------
smc2 <- hbgd::get_smocc_data()[1:2000, ]
smc2$subjid <- as.numeric(as.character(smc2$subjid))
smc2$src <- as.character(smc2$src)
smc2$agedays <- round(smc2$agedays)
smc2$age <- round(smc2$agedays / 365.25, 4)
smc2$gagebrth <- smc2$ga * 7 + 3
smc2$etn <- as.character(smc2$etn)
smc2$birthwt <- smc2$bw
smc2$haz <- round(who_htcm2zscore(smc2$agedays, smc2$htcm, smc2$sex), 3)
smc2$waz <- round(who_wtkg2zscore(smc2$agedays, smc2$wtkg, smc2$sex), 3)
keep <- c("src", "subjid", "rec", "nrec",
          "age", "agedays", "sex", "etn",
          "gagebrth", "birthwt",
          "htcm", "haz", "wtkg", "waz")
smc2 <- smc2[, keep]
identical(smc, smc2)

## ----fig1----------------------------------------------------------------
ids <- c(10001, 10005, 10022)
d <- subset(smc, subjid %in% ids)
idx <- split(d, d$subjid)
figs <- lapply(idx, function(x) {
  figure(xlab = "Age (years)", ylab = "Length (cm)") %>%
  ly_who(x = seq(0, 750, by = 30), y_var = "htcm",
    x_trans = days2years, sex = x$sex[1], color = "green",
    p = 100 * pnorm(-c(2.5,2,1,0))) %>%
  ly_lines(days2years(x$agedays), x$htcm,
    col = "grey", hover = c(x$age, x$htcm)) %>%
  ly_points(days2years(x$agedays), x$htcm,
    col = "blue", hover = c(x$age, x$htcm), size = 6)
})
grid_plot(figs, same_axes = TRUE, simplify_axes = TRUE, width = 680, height = 300)

## ----fig2----------------------------------------------------------------
figs <- lapply(idx, function(x) {
  figure(xlab = "Age (years)", ylab = "Length (SDS)") %>%
  ly_zband(x = days2years(seq(0, 750, by = 30)), z = -c(2.5,2,1,0)) %>%
  ly_lines(x$age, x$haz, col = "grey", hover = c(x$age, x$haz)) %>%
  ly_points(x$age, x$haz,
    col = "blue", hover = c(x$age, x$haz), size = 6)
})
grid_plot(figs, same_axes = TRUE, simplify_axes = TRUE, width = 680, height = 300)

## ------------------------------------------------------------------------
get_knots(fit1)

## ------------------------------------------------------------------------
fit1

## ------------------------------------------------------------------------
p1 <- predict(fit1)
dim(p1)
head(p1, 4)

## ------------------------------------------------------------------------
p2 <- predict(fit1, at = "knots")
head(p2, 4)

## ------------------------------------------------------------------------
pr <- predict(fit1, x = get_knots(fit1))
head(pr, 15)

## ----fig3----------------------------------------------------------------
d <- subset(pr, subjid %in% ids & x <= 2.2)
idx <- split(d, d$subjid)
figs <- lapply(idx, function(x) {
  figure(xlab = "Age (years)", ylab = "Length (SDS)") %>%
  ly_zband( x = days2years(seq(0, 750, by = 30)), z = -c(2.5,2,1,0)) %>%
  ly_lines( x = x$x[!x$knot], y = x$y[!x$knot], color = "grey") %>%
  ly_points(x = x$x[!x$knot], x$y[!x$knot], color = "blue", size = 6) %>%
  ly_lines( x = x$x[x$knot], y = x$yhat[x$knot], col = "darkred") %>%
  ly_points(x = x$x[x$knot], x$yhat[x$knot], col = "red", size = 6) 
  })
grid_plot(figs, same_axes = TRUE, simplify_axes = TRUE, 
          width = 680, height = 300)

## ------------------------------------------------------------------------
pr <- predict(fit2, x = get_knots(fit_hgt))
head(pr, 4)

## ----echo=FALSE----------------------------------------------------------
d <- subset(pr, subjid %in% ids & x <= 2.2)
idx <- split(d, d$subjid)
figs <- lapply(idx, function(x) {
  figure(xlab = "Age (years)", ylab = "Length (SDS)") %>%
  ly_zband( x = days2years(seq(0, 750, by = 30)), z = -c(2.5,2,1,0)) %>%
  ly_lines( x = x$x[!x$knot], y = x$y[!x$knot], color = "grey") %>%
  ly_points(x = x$x[!x$knot], x$y[!x$knot], color = "blue", size = 6) %>%
  ly_lines( x = x$x[x$knot], y = x$yhat[x$knot], col = "darkred") %>%
  ly_points(x = x$x[x$knot], x$yhat[x$knot], col = "red", size = 6) 
  })
grid_plot(figs, same_axes = TRUE, simplify_axes = TRUE, 
          width = 680, height = 300)

## ------------------------------------------------------------------------
var(fitted(fit1), na.rm = TRUE) / var(smc$haz, na.rm = TRUE)

## ------------------------------------------------------------------------
var(fitted(fit2), na.rm = TRUE) / var(smc$haz, na.rm = TRUE)

## ------------------------------------------------------------------------
# export the broken stick models
exp <- export(fit2)
attributes(exp)
lapply(exp, round, 2)

## ------------------------------------------------------------------------
# four height measurement on new child
x <- c(0, 0.12, 0.32, 0.62, 1.1)
y <- c(-1.2, -1.8, -1.7, -1.9, -2.1)

# prediction at ages x
atx <- predict(exp, y = y, x = x)
atx
# prediction at the knots
atknots <- predict(exp, y = y, x = x, at = "knots")
head(atknots)

## ----echo = FALSE, fig.align = "center"----------------------------------
pr <- rbind(atx, atknots)
d <- subset(pr, x <= 2.2)
idx <- list(d)
figs <- lapply(idx, function(x) {
  if (is.null(x)) return(NULL)
  figure(xlab = "Age (years)", ylab = "Length (SDS)") %>%
  ly_zband( x = days2years(seq(0, 750, by = 30)), z = -c(2.5,2,1,0)) %>%
  ly_lines( x = x$x[!x$knot], y = x$y[!x$knot], color = "grey") %>%
  ly_points(x = x$x[!x$knot], x$y[!x$knot], color = "blue", size = 10) %>%
  ly_lines( x = x$x[x$knot], y = x$yhat[x$knot], col = "red") %>%
  ly_points(x = x$x[x$knot], x$yhat[x$knot], col = "red", size = 10) 
  })

grid_plot(figs)

