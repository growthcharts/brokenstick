## ------------------------------------------------------------------------
library("brokenstick")
data <- smocc.hgtwgt
head(data)

## ----fig.width = 6-------------------------------------------------------
library(lattice)
xyplot(hgt ~ age | id, data = data, 
	   subset = id %in% c(10001, 10005), 
	   type = "b", pch = 19, as.table = TRUE)

## ----fig.width = 6-------------------------------------------------------
xyplot(hgt.z ~ age | id, data = data, 
	   subset = id %in% c(10001, 10005), 
	   type = "b", pch = 19, as.table = TRUE,
	   panel = function(...) {
	   	panel.refline(h = c(-2, 0, 2))
	   	panel.xyplot(...)
	   }
)

## ------------------------------------------------------------------------
fit1 <- brokenstick(y = data$hgt.z, 
					x = data$age,
					subject = data$id,
					knots = 0:2)

## ------------------------------------------------------------------------
class(fit1)
fit1@knots
summary(fit1)

## ------------------------------------------------------------------------
est <- conditional.means(fit1)
head(est)

## ----fig.width = 6-------------------------------------------------------
library("dplyr")
colnames(est)[length(colnames(est))] <- "hgt.z"
est <- data.frame(est, src = "smocc", bse = TRUE)
data2 <- bind_rows(data.frame(data, bse = FALSE), est)

xyplot(hgt.z ~ age | id, data = data2,
       groups = bse, 
	     subset = id %in% c(10001, 10005) & age <= 2.3, 
	   type = "b", pch = 19, as.table = TRUE,
	   panel = function(...) {
	   	panel.refline(h = c(-2, 0, 2))
	   	panel.refline(v = 0:2, col = "firebrick1", lwd = 0.5, lty = 2)
	   	panel.xyplot(...)
	   }
)

## ------------------------------------------------------------------------
knots <- c(0, 5/12, 1, 2)
fit2 <- brokenstick(y = data$hgt.z, 
					x = data$age,
					subject = data$id,
					knots = knots)

## ----echo=FALSE, fig.width = 6-------------------------------------------
est <- conditional.means(fit2)
colnames(est)[length(colnames(est))] <- "hgt.z"
est <- data.frame(est, src = "smocc", bse = TRUE)
data2 <- bind_rows(data.frame(data, bse = FALSE), est)

xyplot(hgt.z ~ age | id, data = data2,
       groups = bse, 
	     subset = id %in% c(10001, 10005) & age <= 2.3, 
	   type = "b", pch = 19, as.table = TRUE,
	   panel = function(...) {
	   	panel.refline(h = c(-2, 0, 2))
	   	panel.refline(v = knots, col = "firebrick1", lwd = 0.5, lty = 2)
	   	panel.xyplot(...)
	   }
)

## ------------------------------------------------------------------------
var(fitted(fit1)) / var(data$hgt.z)

## ------------------------------------------------------------------------
var(fitted(fit2)) / var(data$hgt.z)

## ------------------------------------------------------------------------
# export the broken stick models
export.hgt <- export.brokenstick(fit1)
export.hgt

## ------------------------------------------------------------------------
# repredict first observation using three-line model
d <- data[data$id == "10001", c("age","hgt.z")]
predict(fit2, y = d$hgt.z, age = d$age)

## ------------------------------------------------------------------------
# trajectory at +1 and at -1
predict(fit2, y = rep(1, 4), age = c(0, 1/2, 1, 2))
predict(fit2, y = rep(0, 4), age = c(0, 1/2, 1, 2))
predict(fit2, y = rep(-1, 4), age = c(0, 1/2, 1, 2))

## ------------------------------------------------------------------------
# trajectory at +1 and at -1, but only at birth
predict(fit2, y = 1, age = 0)
predict(fit2, y = -1, age = 0)

