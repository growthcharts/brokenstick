## ------------------------------------------------------------------------
library(brokenstick)
data <- smocc.hgtwgt
head(data)

## ----fig.width = 7-------------------------------------------------------
library(lattice)
xyplot(hgt ~ age | as.factor(id), data = data, 
	   subset = id %in% c(10001, 10005), 
	   type = "b", pch = 19, as.table = TRUE)

## ----fig.width = 7-------------------------------------------------------
xyplot(hgt.z ~ age | as.factor(id), data = data, 
	   subset = id %in% c(10001, 10005), 
	   type = "b", pch = 19, as.table = TRUE,
	   panel = function(...) {
	   	panel.refline(h = c(-2, 0, 2))
	   	panel.xyplot(...)
	   }
)

## ---- echo = FALSE-------------------------------------------------------
fit1 <- fit.brokenstick(z = data$hgt.z, 
							 age = data$age,
							 id = data$id,
							 knots = 0:2)
est <- get.brokenstick.values(fit1)
data2 <- rbind(data)
xyplot(hgt.z ~ age | as.factor(id), data = data, 
	   subset = id %in% c(10001, 10005), 
	   type = "b", pch = 19, as.table = TRUE,
	   panel = function(...) {
	   	panel.refline(h = c(-2, 0, 2))
	   	panel.refline(v = 0:3, col = "firebrick1", lty = 2)
	   	panel.xyplot(...)
	   }
)

## ------------------------------------------------------------------------
X <- make.basis(x = data$age, knots = 0:2)
head(data.frame(age = data$age, X))

## ------------------------------------------------------------------------
fit2lines <- fit.brokenstick(z = data$hgt.z, 
							 age = data$age,
							 id = data$id,
							 knots = 0:2)
class(fit2lines)
attr(fit2lines, "knots")

## ------------------------------------------------------------------------
fit2lines

## ------------------------------------------------------------------------
var(fitted(fit2lines)) / var(data$hgt.z)

## ------------------------------------------------------------------------
# export the broken stick models
export.hgt <- export.brokenstick(fit2lines)
export.hgt

