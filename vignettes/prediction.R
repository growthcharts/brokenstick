## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 5)

## ----data----------------------------------------------------------------
library("brokenstick")
head(smocc.hgtwgt)
data <- smocc.hgtwgt[1:2000,]
head(data)

## ----zscores-------------------------------------------------------------
library(AGD)
data$HAZ <- y2z(y = data$hgt, 
				  x = data$age, 
				  sex = ifelse(data$sex == "female", "F", "M"),
				  ref = get("who.hgt", pos = "package:AGD"))

## ------------------------------------------------------------------------
hist(data$HAZ)

## ------------------------------------------------------------------------
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
Boundary.knots <- c(0, 3)

## ------------------------------------------------------------------------
fit <- brokenstick(y = data$HAZ,
                   x = data$age,
                   subject = data$id,
                   knots = knots,
                   Boundary.knots = Boundary.knots)

## ------------------------------------------------------------------------
class(fit)
slotNames(fit)
fit.exp <- export.brokenstick(fit)
names(fit.exp)

## ------------------------------------------------------------------------
UID <- unique(data$id)
cid <- UID[1]
d <- data[data$id == cid, ]
b <- predict(fit.exp, d$HAZ, d$age)

plot(x = d$age, y = d$HAZ, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Break ages, child", cid))
brk <- c(fit.exp$knots, fit.exp$Boundary.knots[2])
brk
lines(x = brk, y = b, col = "red", lwd = 2, lty = 2, type = "b")
abline(v = brk, lty = 5, col = "grey80")

## ------------------------------------------------------------------------
b <- predict(fit.exp, d$HAZ, d$age, type = "response")

plot(x = d$age, y = d$HAZ, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Measurement ages, child", cid))
points(x = d$age, y = b, col = "red", lwd = 2, lty = 2, type = "b")
abline(v = d$age, lty = 5, col = "grey80")

## ------------------------------------------------------------------------
age <- c(d$age, c(0.5, 14, 17)/12)
y <- c(d$HAZ, c(NA, NA, NA))
b <- predict(fit.exp, y, age, type = "response")

plot(x = d$age, y = d$HAZ, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Arbitrary ages, child", cid))
idx <- -(1:length(d$age))
points(x = age[idx], y = b[idx], col = "red", lwd = 2, lty = 2, type = "b")
abline(v = age[idx], lty = 5, col = "grey80")

## ------------------------------------------------------------------------
age <- d$age[1:5]
y <- d$HAZ[1:5]
b <- predict(fit.exp, y, age)

plot(age, y, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Future trajectory, child", cid), xlim = c(0,2), ylim = c(0, 1.5))
lines(x = brk, y = b, col = "red", lwd = 2, lty = 2, type = "b")
abline(v = brk, lty = 5, col = "grey80")

## ----fig.height=7, fig.width=7-------------------------------------------
ds <- split(data, f = data$id)
result <- vector("list", length(ds))

for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(fit.exp, d$HAZ, d$age, type = "response")
}

data$zhat <- unlist(result)

## ------------------------------------------------------------------------
library("MASS")
eqscplot(x = data$zhat, y = data$HAZ, xlab = "Predicted Z-score", ylab = "Observed Z-score", main = "All children")
abline(0, 1, col = "blue")

## ------------------------------------------------------------------------
# convert Z-score to CM
data$yhat <- z2y(z = data$zhat, 
                 x = data$age,
                 sex = ifelse(data$sex == "female", "F", "M"),
                 ref = get("who.hgt", pos = "package:AGD"))

## ----echo = FALSE--------------------------------------------------------
eqscplot(x = data$yhat, y = data$hgt, xlab = "Predicted (cm)", 
         ylab = "Observed (cm)", main = "All children")
abline(0, 1, col = "blue")

## ----echo = FALSE--------------------------------------------------------
data <- smocc.hgtwgt[-(1:2000), ]
data$HAZ <- y2z(y = data$hgt, 
				  x = data$age, 
				  sex = ifelse(data$sex == "female", "F", "M"),
				  ref = get("who.hgt", pos = "package:AGD"))

ds <- split(data, f = data$id)
result <- vector("list", length(ds))

for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(fit.exp, d$HAZ, d$age, type = "response")
}

data$zhat <- unlist(result)

## ------------------------------------------------------------------------
eqscplot(x = data$zhat, y = data$HAZ, xlab = "Predicted Z-score", ylab = "Observed Z-score", main = "Holdout sample")
abline(0, 1, col = "blue")

## ----echo = FALSE--------------------------------------------------------
# convert Z-score to CM
data$yhat <- z2y(z = data$zhat, 
                 x = data$age,
                 sex = ifelse(data$sex == "female", "F", "M"),
                 ref = get("who.hgt", pos = "package:AGD"))

## ----echo = FALSE--------------------------------------------------------
eqscplot(x = data$yhat, y = data$hgt, xlab = "Predicted (cm)", 
         ylab = "Observed (cm)", main = "Holdout sample")
abline(0, 1, col = "blue")

## ------------------------------------------------------------------------
(fit.exp <- export.brokenstick(fit.hgt))

## ----echo = FALSE--------------------------------------------------------
data <- smocc.hgtwgt
data$HAZ <- y2z(y = data$hgt, 
				  x = data$age, 
				  sex = ifelse(data$sex == "female", "F", "M"),
				  ref = get("who.hgt", pos = "package:AGD"))

ds <- split(data, f = data$id)
result <- vector("list", length(ds))

for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(fit.exp, d$HAZ, d$age, type = "response")
}

data$zhat <- unlist(result)

## ----echo = FALSE--------------------------------------------------------
# convert Z-score to CM
data$yhat <- z2y(z = data$zhat, 
                 x = data$age,
                 sex = ifelse(data$sex == "female", "F", "M"),
                 ref = get("who.hgt", pos = "package:AGD"))

