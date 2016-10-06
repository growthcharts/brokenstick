# smocc1.R
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

# distribution of Z-score by ahe
with(data, plot(age, hgt.z, main = "Dutch 1989-1990, height relative to WHO"))
abline(h = c(-2,0,2), col = "grey")
with(data, lines(loess.smooth(x = age, y = hgt.z, span = 0.2), col = "red", lwd = 2))

# remove outliers and records with missing ages and/or heights
trim <- with(data, !is.na(age) & !is.na(hgt) & hgt.z > (-5) & hgt.z < 5)
d <- data[trim, ]

# fit the brokenstick model
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
boundary <- c(0, 3)
fit.hgt <- with(d,
				brokenstick(y = hgt.z,
							x = age,
							subjid = id,
							knots = knots,
							boundary = boundary))

# store 'smocc.hgtwgt' for lazy loading
smocc.hgtwgt <- data
fn1 <- path.expand("~/Package/brokenstick/brokenstick/data/smocc.hgtwgt.rda")
save(smocc.hgtwgt, file = fn1, compress = "xz")

# store 'fit.hgt' for lazy loading
fn2 <- path.expand("~/Package/brokenstick/brokenstick/data/fit.hgt.rda")
save(fit.hgt, file = fn2, compress = "xz")


