## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 3.5)

## ----data----------------------------------------------------------------
require("brokenstick")
require("hbgd")
require("rbokeh")
smc <- smocc_hgtwgt[1:2000,]

## ----zscores-------------------------------------------------------------
haz <- who_htcm2zscore(smc$agedays, smc$htcm, smc$sex)

## ------------------------------------------------------------------------
htcm <- who_zscore2htcm(smc$agedays, haz, smc$sex)

## ----echo=FALSE----------------------------------------------------------
plot_univar(smc, subject = FALSE, width = 220, height = 220)

## ------------------------------------------------------------------------
figure(xlab = "Age (years)", ylab = get_label("haz")) %>%
  ly_zband(x = seq(0, 2.5, 0.5), z = -c(2.5,2,1,0)) %>%
  ly_points(smc$age, smc$haz, hover = c(smc$age, smc$htcm), size = 4)

## ------------------------------------------------------------------------
plot_visit_distn(smc, width = 350, height = 350)

## ------------------------------------------------------------------------
plot_missing(smc, width = 600, height = 400)

## ------------------------------------------------------------------------
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
boundary <- c(0, 3)

## ------------------------------------------------------------------------
class(fit)

## ------------------------------------------------------------------------
round(lme4::fixef(fit), 2)
round(get_knots(fit), 2)

## ------------------------------------------------------------------------
figure(xlab = "Age (years)", ylab = get_label("haz"), title = "Mean trajectory (n = 206)") %>%
  ly_zband(x = seq(0, 2.5, 0.5), z = -c(2.5,2,1,0)) %>%
  ly_points(get_knots(fit), lme4::fixef(fit), hover = c(get_knots(fit), lme4::fixef(fit))) %>%
  ly_lines(get_knots(fit), lme4::fixef(fit))

## ------------------------------------------------------------------------
fit

## ------------------------------------------------------------------------
p1 <- predict(fit)
head(p1, 4)

## ------------------------------------------------------------------------
p2 <- predict(fit, at = "knots")
head(p2, 4)

## ------------------------------------------------------------------------
p3 <- predict(fit, at = "both")
head(p3, 4)

## ------------------------------------------------------------------------
plot(fit, height = 300, size.y = 4, size.yhat = 4, x_trim = c(0, 2.2))

## ------------------------------------------------------------------------
p1 <- predict(fit, ids = 10001, at = "knots", output = "vector")

## ----fig3----------------------------------------------------------------
plot(fit, ids = 10001, x_trim = c(0, 2.2))

## ------------------------------------------------------------------------
# convert Z-score to CM
p <- predict(fit, ids = 10001, at = "both")
p$ycm <- round(who_zscore2htcm(years2days(p$x), p$y, sex = "Female"), 1)
p$yhatcm <- who_zscore2htcm(years2days(p$x), p$yhat, sex = "Female")
head(p)

## ------------------------------------------------------------------------
p <- p[p$x <= 2.5,]
figure(xlab = "Age (years)", ylab = get_label("htcm")) %>%
  ly_who(x = seq(0, 750, by = 30), y_var = "htcm",
    x_trans = days2years, sex = "Female", color = "green",
    p = 100 * pnorm(-c(2.5,2,1,0))) %>%
  ly_points(p$x, p$ycm) %>%
  ly_lines(p$x[p$knot], p$yhatcm[p$knot], col = "orangered") %>%
  ly_points(p$x[p$knot], p$yhatcm[p$knot], col = "orangered", size = 6)

## ------------------------------------------------------------------------
# export the broken stick models
exp <- export(fit)
attributes(exp)
lapply(exp, round, 2)

## ------------------------------------------------------------------------
# Five age-haz observations for Fred
x <- c(0, 0.12, 0.32, 0.62, 1.1)
y <- c(-1.2, -1.8, -1.7, -1.9, -2.1)
predict(exp, y, x, at = "both", subjid = "Fred")

## ----echo = FALSE, fig.align = "center"----------------------------------
plot(exp, y, x, at = "both", x_trim = c(0, 2.2))

## ----fig.height=7, fig.width=7-------------------------------------------
# if we have access to the brokenstick object
p <- predict(fit)
head(p, 3)

## ------------------------------------------------------------------------
# case-by-case prediction on external data 
ds <- split(smc, f = smc$subjid, drop = TRUE)
result <- vector("list", length(ds))
for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(exp, y = d$haz, x = d$age, subjid = d$subjid[1])
}
result <- do.call(rbind, result)
row.names(result) <- 1:nrow(result)

# do we get same answers?
all.equal(result, p)

