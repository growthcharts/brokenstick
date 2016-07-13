## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 5)

## ----data----------------------------------------------------------------
library("brokenstick")
data <- smocc.hgtwgt[1:2000,]
head(data)

## ----zscores-------------------------------------------------------------
library("AGD")
data$HAZ <- y2z(y = data$hgt, 
                x = data$age, 
                sex = ifelse(data$sex == "female", "F", "M"),
                ref = get("who.hgt", pos = "package:AGD"))

## ----echo=FALSE----------------------------------------------------------
hist(data$HAZ, main = "SMOCC data, first 2000 records", 
     xlab = "Height (Z-score)")

## ----echo = FALSE--------------------------------------------------------
plot(x = data$age, y = data$HAZ, xlab = "Age (in years)", ylab = "Height SDS", main = "SMOCC data, first 2000 records", pch = 20, cex = 0.5) 

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
p <- predict(fit.exp, y = d$HAZ, x = d$age, type = "atknots")

plot(x = d$age, y = d$HAZ, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Break ages, child", cid))
lines(x = p$x, y = p$yhat, col = "red", lwd = 1.5, lty = 2, type = "b")
abline(v = p$x, lty = 5, col = "grey80")

## ------------------------------------------------------------------------
p <- predict(fit.exp, y = d$HAZ, x = d$age)

## ----echo = FALSE--------------------------------------------------------
plot(x = d$age, y = d$HAZ, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Measurement ages, child", cid))
points(x = p$x, y = p$yhat, col = "red", lwd = 1.5, lty = 2, type = "b")
abline(v = d$age, lty = 5, col = "grey80")

## ------------------------------------------------------------------------
age <- c(d$age, c(0.5, 14, 17)/12)
y <- c(d$HAZ, c(NA, NA, NA))
p <- predict(fit.exp, y = y, x = age)

## ----echo = FALSE--------------------------------------------------------
plot(x = d$age, y = d$HAZ, type = "b", col = "blue", 
     xlab = "Age (yrs)", ylab = "HAZ", main = paste("Arbitrary ages, child", cid))
points(x = p$x[p$new], y = p$yhat[p$new], col = "red", lwd = 1.5, lty = 2, type = "b")
abline(v = p$x[p$new], lty = 5, col = "grey80")

## ------------------------------------------------------------------------
age <- d$age[1:5]
y <- d$HAZ[1:5]
p <- predict(fit.exp, y = y, x = age, type = "atknots")

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
ds <- split(data, f = data$id, drop = TRUE)
result <- vector("list", length(ds))

for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(fit.exp, y = d$HAZ, x = d$age, 
                                          output = "vector", 
                                          include.boundaries = TRUE)
}
data$yhat <- unlist(result)
head(data)

# do we get same answers?
all.equal(data$yhat, p$yhat)

## ------------------------------------------------------------------------
library("MASS")
eqscplot(y = data$yhat, x = data$HAZ, ylab = "Predicted Z-score", xlab = "Observed Z-score", pch = ".", main = "Children in the model")
abline(0, 1, col = "grey")

## ------------------------------------------------------------------------
# convert Z-score to CM
data$yhatcm <- z2y(z = data$yhat, 
                   x = data$age,
                   sex = ifelse(data$sex == "female", "F", "M"),
                   ref = get("who.hgt", pos = "package:AGD"))

## ----echo = FALSE--------------------------------------------------------
eqscplot(y = data$yhatcm, x = data$hgt, pch = ".", ylab = "Predicted (cm)", 
         xlab = "Observed (cm)", main = "Children in the model")
abline(0, 1, col = "grey")

## ----echo = FALSE--------------------------------------------------------
data <- smocc.hgtwgt[-(1:2000), ]
data$HAZ <- y2z(y = data$hgt, 
                x = data$age, 
                sex = ifelse(data$sex == "female", "F", "M"),
                ref = get("who.hgt", pos = "package:AGD"))

ds <- split(data, f = data$id, drop = TRUE)
result <- vector("list", length(ds))

for (i in seq_along(ds)) {
  d <- ds[[i]]
  if (nrow(d) > 0) result[[i]] <- predict(fit.exp, y = d$HAZ, x = d$age, 
                                          output = "vector", 
                                          include.boundaries = TRUE)
}
data$yhat <- unlist(result)

## ----echo=FALSE----------------------------------------------------------
eqscplot(y = data$yhat, x = data$HAZ, pch = ".", ylab = "Predicted Z-score", xlab = "Observed Z-score", main = "Holdout sample")
abline(0, 1, col = "grey")

## ----echo = FALSE--------------------------------------------------------
# convert Z-score to CM
data$yhatcm <- z2y(z = data$yhat, 
                 x = data$age,
                 sex = ifelse(data$sex == "female", "F", "M"),
                 ref = get("who.hgt", pos = "package:AGD"))

## ----echo = FALSE--------------------------------------------------------
eqscplot(y = data$yhatcm, x = data$hgt, pch = ".", ylab = "Predicted (cm)", 
         xlab = "Observed (cm)", main = "Holdout sample")
abline(0, 1, col = "grey")

