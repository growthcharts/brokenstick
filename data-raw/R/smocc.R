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

# make a factor of id
data$id <- as.factor(data$id)

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
knots <- round(c(0, 28/365.25, 56/365.25, 
				 1/4, 1/3, 1/2, 7.5/12,
				 9/12, 11/12, 14/12, 18/12, 2), 4)
fit.hgt <- with(data, 
				brokenstick(y = hgt.z, 
							x = age, 
							subject = id,
							knots = knots))

# store 'smocc.hgtwgt' for lazy loading
smocc.hgtwgt <- data
fn1 <- path.expand("~/Package/brokenstick/brokenstick/data/smocc.hgtwgt.rda")
save(smocc.hgtwgt, file = fn1, compress = "xz")

# store 'fit.hgt' for lazy loading
fn2 <- path.expand("~/Package/brokenstick/brokenstick/data/fit.hgt.rda")
save(fit.hgt, file = fn2, compress = "xz")


