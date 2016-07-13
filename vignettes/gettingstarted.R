## ----echo = FALSE--------------------------------------------------------
library("knitr")
opts_chunk$set(fig.width = 7, fig.height = 3.5)

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
summary(fit1)
get.knots(fit1, include.boundaries = TRUE)

## ------------------------------------------------------------------------
p1 <- predict(fit1)
dim(p1)
head(p1, 4)

## ------------------------------------------------------------------------
p2 <- predict(fit1, type = "atknots")
head(p2, 4)

## ------------------------------------------------------------------------
p <- predict(fit1, x = get.knots(fit1), include.boundaries = TRUE)
head(p, 15)

## ------------------------------------------------------------------------
p$yhat[p$knot] <- NA
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

## ------------------------------------------------------------------------
p <- predict(fit2, x = get.knots(fit.hgt), include.boundaries = TRUE)
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
export.hgt

## ------------------------------------------------------------------------
# four height measurement on new child
x <- c(0, 0.15, 0.23, 0.45)
y <- c(-1.2, -1.7, -2.2, -2.3)

# prediction at ages x
atx <- predict(export.hgt, y = y, x = x)
atx
# prediction at the knots
atknots <- predict(export.hgt, y = y, x = x, type = "atknots")
head(atknots)

## ------------------------------------------------------------------------
at <- rbind(atx, atknots)


