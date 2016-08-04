## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 3.5)

## ------------------------------------------------------------------------
library("brokenstick")
data <- smocc.hgtwgt[1:2000,]
head(data)

## ------------------------------------------------------------------------
ids <- c(10001, 10005, 10022)
library(lattice)
xyplot(hgt ~ age | id, data = data, 
	   subset = id %in% ids,  
	   type = "b", pch = 19, as.table = TRUE,
	   xlab = "Age (in years)", ylab = "Length (cm)")

## ------------------------------------------------------------------------
xyplot(hgt.z ~ age | id, data = data, 
	   subset = id %in% ids, 
	   type = "b", pch = 19, as.table = TRUE,
	   panel = function(...) {
	   	panel.refline(h = c(-2, 0, 2))
	   	panel.xyplot(...)
	   },
	   xlab = "Age (in years)", ylab = "Length (SDS)")

## ----fit1----------------------------------------------------------------
knots <- 0:2
fit1 <- brokenstick(y = data$hgt.z, 
					x = data$age,
					subject = data$id,
					knots = knots)

## ------------------------------------------------------------------------
class(fit1)
fit1

## ------------------------------------------------------------------------
p1 <- predict(fit1)
dim(p1)
head(p1, 4)

## ------------------------------------------------------------------------
p2 <- predict(fit1, at = "knots")
head(p2, 4)

## ------------------------------------------------------------------------
p <- predict(fit1, x = get.knots(fit1))
head(p, 15)

## ------------------------------------------------------------------------
p$yhat[!p$knot] <- NA
xyplot(y + yhat ~ x | id, data = p, 
       subset = id %in% ids & x <= 2.3, 
       as.table = TRUE,
       panel = function(...) {
         panel.refline(h = c(-2, 0, 2))
         panel.refline(v = 0:2, col = "grey", lwd = 0.5, lty = 2)
         panel.superpose(...)}, 
       panel.groups = function(x, y, subscripts, groups, ..., group.number) 
         with(na.omit(data.frame(x, y)), 
              panel.lines(x, y, type = "b", pch = 20, col = group.number)),
       xlab = "Age (in years)", ylab = "Length (SDS)")

## ----fit2, cache = TRUE--------------------------------------------------
# 10 scheduled visits
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
Boundary.knots <- c(0, 3)
fit2 <- brokenstick(y = data$hgt.z, 
					x = data$age,
					subject = data$id,
					knots = knots,
					Boundary.knots = Boundary.knots)

## ------------------------------------------------------------------------
p <- predict(fit2, x = get.knots(fit.hgt))
head(p, 4)

## ----echo=FALSE----------------------------------------------------------
p$yhat[!p$knot] <- NA
xyplot(y + yhat ~ x | id, data = p, 
       subset = id %in% ids & x <= 2.3, 
       as.table = TRUE,
       panel = function(...) {
         panel.refline(h = c(-2, 0, 2))
         panel.refline(v = knots, col = "grey", lwd = 0.5, lty = 2)
         panel.superpose(...)}, 
       panel.groups = function(x, y, subscripts, groups, ..., group.number) 
         with(na.omit(data.frame(x, y)), 
              panel.lines(x, y, type = "b", pch = 20, col = group.number)),
       xlab = "Age (in years)", ylab = "Length (SDS)")

## ------------------------------------------------------------------------
var(fitted(fit1)) / var(data$hgt.z)

## ------------------------------------------------------------------------
var(fitted(fit2)) / var(data$hgt.z)

## ------------------------------------------------------------------------
# export the broken stick models
export.hgt <- export.brokenstick(fit2)
attributes(export.hgt)

## ------------------------------------------------------------------------
# four height measurement on new child
x <- c(0, 0.32, 0.62, 1.1)
y <- c(-1.2, -1.7, -1.9, -2.1)

# prediction at ages x
atx <- predict(export.hgt, y = y, x = x)
atx
# prediction at the knots
atknots <- predict(export.hgt, y = y, x = x, at = "knots")
head(atknots)

## ----fig.width = 3, fig.align = "center", echo = FALSE-------------------
p <- rbind(atx, atknots)
p$yhat[!p$knot] <- NA
xyplot(y + yhat ~ x, data = p, ylim = c(-3, 1),
       as.table = TRUE,
       panel = function(...) {
         panel.refline(h = c(-2, 0, 2))
         panel.refline(v = knots, col = "grey", lwd = 0.5, lty = 2)
         panel.superpose(...)}, 
       panel.groups = function(x, y, subscripts, groups, ..., group.number) 
         with(na.omit(data.frame(x, y)), 
              panel.lines(x, y, type = "b", pch = 20, col = group.number)),
       xlab = "Age (in years)", ylab = "Length (SDS)",
       main = "New person, published model")

