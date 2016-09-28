# smocc2.R
#

library(donordata)
if (!require(hbgd)) devtools::install_github("hafen/hbgd")
library(brokenstick)
project <-  path.expand("~/Package/brokenstick/brokenstick")

# Function to pull out form donordata, and convert to hbgd naming
get_smocc_data <- function() {
  from <- c("src", "id", "rec", "nrec", "age", "sex",
            "etn", "ga", "bw", "hgt", "wgt")
  data <- donordata::smocc[[3]][, from]
  to <- c("src", "subjid", "rec", "nrec", "age", "sex",
          "etn", "ga", "birthwt", "htcm", "wtkg")
  rownames(data) <- 1:nrow(data)
  names(data) <- to
  data$src <- as.character(data$src)
  data$agedays <- round(data$age * 365.25)
  data$gagebrth <- data$ga * 7 + 3
  data$sex <- as.character(data$sex)
  data$sex[data$sex == "male"] <- "Male"
  data$sex[data$sex == "female"] <- "Female"
  data$etn <- as.character(data$etn)

  # using the hbgd package
  data$haz <- round(who_htcm2zscore(data$agedays, data$htcm, data$sex), 3)
  data$waz <- round(who_wtkg2zscore(data$agedays, data$wtkg, data$sex), 3)

  # using the AGD package
  # data$haz <- y2z(y = data$htcm,
  #                 x = data$age,
  #                 sex = ifelse(data$sex == "Female", "F", "M"),
  #                 ref = get("who.hgt", pos = "package:AGD"))
  # data$waz <- y2z(y = data$wtkg,
  #                 x = data$age,
  #                 sex = ifelse(data$sex == "Female", "F", "M"),
  #                 ref = get("who.wgt", pos = "package:AGD"))
  keep <- c("src", "subjid", "rec", "nrec",
            "age", "agedays", "sex", "etn",
            "gagebrth", "birthwt",
            "htcm", "haz", "wtkg", "waz")
  return(data[, keep])
  }

data <- get_smocc_data()

# distribution of Z-score by ahe
with(data, plot(age, haz, main = "Dutch 1989-1990, height relative to WHO"))
abline(h = c(-2,0,2), col = "grey")
with(data, lines(loess.smooth(x = age, y = haz, span = 0.2), col = "red", lwd = 2))

# remove outliers and records with missing ages and/or heights
trim <- with(data, !is.na(agedays) & !is.na(haz) & haz > (-5) & haz < 5)
d <- data[trim, ]

# fit the brokenstick model (takes substantial time)
knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
boundary <- c(0, 3)
d$subjid <- as.factor(d$subjid)
fit_hgt <- with(d,
				brokenstick(y = haz,
							x = age,
							subjid = subjid,
							knots = knots,
							boundary = boundary))

# store 'smocc_hgtwgt' for lazy loading
smocc_hgtwgt <- data
fn1 <- path.expand("~/Package/brokenstick/brokenstick/data/smocc_hgtwgt.rda")
save(smocc_hgtwgt, file = fn1, compress = "xz")

# store 'fit_hgt' for lazy loading
fn2 <- path.expand("~/Package/brokenstick/brokenstick/data/fit_hgt.rda")
save(fit_hgt, file = fn2, compress = "xz")

