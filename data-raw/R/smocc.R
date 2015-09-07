# smocc.R
#

library(donordata)
library(AGD)
library(brokenstick)
project <-  path.expand("~/Package/brokenstick/brokenstick")

# copy some columns from the smocc data stored in the donordata package
data <- smocc[[3]][, c("src", "id", "rec", "nrec", "age", "sex", 
					   "etn", "ga", "bw", "hgt", "wgt")]
rownames(data) <- NULL

# calculate Z-scores
data$hgt.z <- y2z(y = data$hgt, 
				  x = data$age, 
				  sex = ifelse(data$sex == "female", "F", "M"),
				  ref = get("who.hgt", pos = "package:AGD"))

# perform broken stick analyses for height
select <- with(data, !is.na(age) & !is.na(hgt) 
			   & (hgt.z >= (-4)) & hgt.z <= 4)
data <- data[select, ]

# with(data, plot(age, hgt.z, main = "Dutch 1989-1990, height relative to WHO"))
# abline(h = c(-2,0,2), col = "grey")
# with(data, lines(lowess(x = age, y = hgt.z), col = "red", lwd = 3))

# fit the brokenstick model
fit.hgt <- with(data, 
				fit.brokenstick(z = hgt.z, 
								age = age, 
								id = id))

# store 'smocc.hgtwgt' for lazy loading
smocc.hgtwgt <- data
fn1 <- path.expand("~/Package/brokenstick/brokenstick/data/smocc.hgtwgt.rda")
save(smocc.hgtwgt, file = fn1, compress = "xz")

# store 'fit.hgt' for lazy loading
fn2 <- path.expand("~/Package/brokenstick/brokenstick/data/fit.hgt.rda")
save(fit.hgt, file = fn2, compress = "xz")


