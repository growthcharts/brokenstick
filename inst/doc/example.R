## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 3.5)

## ------------------------------------------------------------------------
require("brokenstick")
smc <- smocc_hgtwgt[1:2000, ]
head(smc)

## ------------------------------------------------------------------------
if (!require(hbgd)) devtools::install_github("HBGDki/hbgd")
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

## ------------------------------------------------------------------------
ids <- c(10001, 10005, 10022)
require("lattice")
xyplot(htcm ~ age | subjid, data = smc, 
	   subset = subjid %in% ids,  
	   type = "b", pch = 19, as.table = TRUE,
	   xlab = "Age (in years)", ylab = "Length (cm)")

## ------------------------------------------------------------------------
xyplot(haz ~ age | subjid, data = smc, 
	   subset = subjid %in% ids, 
	   type = "b", pch = 19, as.table = TRUE,
	   panel = function(...) {
	   	panel.refline(h = c(-2, 0, 2))
	   	panel.xyplot(...)
	   },
	   xlab = "Age (in years)", ylab = "Length (SDS)")

## ----fit1----------------------------------------------------------------
knots <- 0:2
fit1 <- brokenstick(y = smc$haz, 
					x = smc$age,
					subject = smc$subjid,
					knots = knots)

## ------------------------------------------------------------------------
get_knots(fit1)

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
p <- predict(fit1, x = get_knots(fit1))
head(p, 15)

## ------------------------------------------------------------------------
p$yhat[!p$knot] <- NA
xyplot(y + yhat ~ x | subjid, data = p, 
       subset = subjid %in% ids & x <= 2.3, 
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
p <- predict(fit2, x = get_knots(fit_hgt))
head(p, 4)

## ----echo=FALSE----------------------------------------------------------
p$yhat[!p$knot] <- NA
xyplot(y + yhat ~ x | subjid, data = p, 
       subset = subjid %in% ids & x <= 2.3, 
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
var(fitted(fit1), na.rm = TRUE) / var(smc$haz, na.rm = TRUE)

## ------------------------------------------------------------------------
var(fitted(fit2), na.rm = TRUE) / var(smc$haz, na.rm = TRUE)

## ------------------------------------------------------------------------
# export the broken stick models
export_hgt <- export(fit2)
attributes(export_hgt)
lapply(export_hgt, round, 2)

## ------------------------------------------------------------------------
# four height measurement on new child
x <- c(0, 0.12, 0.32, 0.62, 1.1)
y <- c(-1.2, -1.8, -1.7, -1.9, -2.1)

# prediction at ages x
atx <- predict(export_hgt, y = y, x = x)
atx
# prediction at the knots
atknots <- predict(export_hgt, y = y, x = x, at = "knots")
head(atknots)

## ----fig.width = 3, fig.align = "center", echo = FALSE-------------------
p <- rbind(atx, atknots)
p$yhat[!p$knot] <- NA
xyplot(y + yhat ~ x, data = p, ylim = c(-3, 1), xlim = c(-0.1, 2.1),
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

