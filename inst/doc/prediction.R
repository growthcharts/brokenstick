## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 5)

## ----data----------------------------------------------------------------
library("brokenstick")
smc <- smocc_hgtwgt[1:2000,]
head(smc)

## ----zscores-------------------------------------------------------------
if (!require(hbgd)) devtools::install_github("hafen/hbgd")
smc$haz <- round(who_htcm2zscore(smc$agedays, smc$htcm, smc$sex), 3)

## ----echo=FALSE----------------------------------------------------------
hist(smc$haz, main = "SMOCC data, first 2000 records", 
     xlab = "Height (Z-score)")

## ----echo = FALSE--------------------------------------------------------
plot(x = smc$age, y = smc$haz, xlab = "Age (in years)", ylab = "Height SDS", main = "SMOCC data, first 2000 records", pch = 20, cex = 0.5) 

## ------------------------------------------------------------------------
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
boundary <- c(0, 3)

## ------------------------------------------------------------------------
fit <- brokenstick(y = smc$haz,
                   x = smc$age,
                   subjid = smc$subjid,
                   knots = knots,
                   boundary = boundary)

## ------------------------------------------------------------------------
class(fit)
slotNames(fit)
exp <- export(fit)
names(exp)

## ------------------------------------------------------------------------
UID <- unique(smc$subjid)
cid <- UID[1]
d <- smc[smc$subjid == cid, ]
p <- predict(exp, y = d$haz, x = d$age, at = "knots")

plot(x = d$age, y = d$haz, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Break ages, child", cid))
lines(x = p$x, y = p$yhat, col = "red", lwd = 1.5, lty = 2, type = "b")
abline(v = p$x, lty = 5, col = "grey80")

## ------------------------------------------------------------------------
p <- predict(exp, y = d$haz, x = d$age)

## ----echo = FALSE--------------------------------------------------------
plot(x = d$age, y = d$haz, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Measurement ages, child", cid))
points(x = p$x, y = p$yhat, col = "red", lwd = 1.5, lty = 2, type = "b")
abline(v = d$age, lty = 5, col = "grey80")

## ------------------------------------------------------------------------
age <- c(d$age, c(0.5, 14, 17)/12)
y <- c(d$haz, c(NA, NA, NA))
p <- predict(exp, y = y, x = age)

## ----echo = FALSE--------------------------------------------------------
plot(x = d$age, y = d$haz, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Arbitrary ages, child", cid))
points(x = p$x[p$new], y = p$yhat[p$new], col = "red", lwd = 1.5, lty = 2, type = "b")
abline(v = p$x[p$new], lty = 5, col = "grey80")

## ------------------------------------------------------------------------
age <- d$age[1:5]
y <- d$haz[1:5]
p <- predict(exp, y = y, x = age, at = "knots")

## ----echo = FALSE--------------------------------------------------------
plot(age, y, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Future trajectory, child", cid), xlim = c(0,2), ylim = c(0, 1.5))
lines(x = p$x, y = p$yhat, col = "red", lwd = 1.5, lty = 2, type = "b")
abline(v = p$x, lty = 5, col = "grey80")

## ----fig.height=7, fig.width=7-------------------------------------------
# if we have access to the brokenstick object
p <- predict(fit)
dim(p)
head(p)

# case-by-case prediction on external data, 
ds <- split(smc, f = smc$subjid, drop = TRUE)
result <- vector("list", length(ds))

for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(exp, y = d$haz, x = d$age, 
                                          output = "vector", 
                                          include.boundaries = TRUE)
}
smc$yhat <- unlist(result)
head(smc)

# do we get same answers?
all.equal(smc$yhat, p$yhat)

## ------------------------------------------------------------------------
library("MASS")
eqscplot(y = smc$yhat, x = smc$haz, ylab = "Predicted Z-score", xlab = "Observed Z-score", pch = ".", main = "Children in the model")
abline(0, 1, col = "grey")

## ------------------------------------------------------------------------
# convert Z-score to CM
smc$yhatcm <- who_zscore2htcm(smc$agedays, smc$yhat, smc$sex)

## ----echo = FALSE--------------------------------------------------------
eqscplot(y = smc$yhatcm, x = smc$htcm, pch = ".", ylab = "Predicted (cm)", 
         xlab = "Observed (cm)", main = "Children in the model")
abline(0, 1, col = "grey")

## ----echo = FALSE--------------------------------------------------------
smc <- smocc_hgtwgt[-(1:2000), ]
smc$haz <- round(who_htcm2zscore(smc$agedays, smc$htcm, smc$sex), 3)

ds <- split(smc, f = smc$subjid, drop = TRUE)
result <- vector("list", length(ds))

for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(exp, y = d$haz, x = d$age, 
                                          output = "vector", 
                                          include.boundaries = TRUE)
}
smc$yhat <- unlist(result)

## ----echo=FALSE----------------------------------------------------------
eqscplot(y = smc$yhat, x = smc$haz, pch = ".", ylab = "Predicted Z-score", xlab = "Observed Z-score", main = "Holdout sample")
abline(0, 1, col = "grey")

## ----echo = FALSE--------------------------------------------------------
# convert Z-score to CM
smc$yhatcm <- who_zscore2htcm(smc$agedays, smc$yhat, smc$sex)

## ----echo = FALSE--------------------------------------------------------
eqscplot(y = smc$yhatcm, x = smc$htcm, pch = ".", ylab = "Predicted (cm)", 
         xlab = "Observed (cm)", main = "Holdout sample")
abline(0, 1, col = "grey")

