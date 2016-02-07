## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github(repo = "stefvanbuuren/brokenstick", oath = Sys.getenv("GITHUB_PAT"))

## ----eval = FALSE--------------------------------------------------------
#  install.packages(pkgs, repos = NULL, type = "source")

## ----eval = FALSE--------------------------------------------------------
#  options(repos = c(tessera = "http://packages.tessera.io", getOption("repos")))
#  install.packages("hbgd")

## ------------------------------------------------------------------------
library("brokenstick")
head(smocc.hgtwgt)

## ------------------------------------------------------------------------
holdout <- function(data, random=TRUE){
  
  colnames(data)[1:3] <- c("ID", "age", "y")
  
  ID <- data$ID
  row <- 1:nrow(data)
  
  
  samplerandom <- function(x){
    if(length(x)==1){
      0
    }else{
      sample(x,1) 
    }
  }
  
  samplemax <- function(x){
    if(length(x)==1){
      0
    }else{
      max(x)
    }
  }
  
  
  if(random==TRUE){
    validation.set <- tapply(X=row,INDEX=ID,FUN=samplerandom)
  }else{
    if(random==FALSE){
      validation.set <- tapply(X=row,INDEX=ID,FUN=samplemax)
    }
  }
  
  hold <- row %in% validation.set
  
  return(hold)
}

## ------------------------------------------------------------------------
data <- smocc.hgtwgt[1:2000, c("id", "age", "hgt.z", "hgt", "sex")]

# generate random holdout sample
set.seed(63621)
data <- data.frame(data, hold = holdout(data))
head(data)
summary(data)

## ------------------------------------------------------------------------
  #Rename columns of data matrix
  colnames(data) <- c("id", "age", "y", "hgt", "sex", "hold")
  
  UID <- unique(data$id)

  # instead of uniform knots: knots=c(0.2, 0.4, 0.6, 0.8)
  # take four internal knots at 20th, 40th, 60th and 80th age quantile
  knots <- quantile(data$age, seq(0, 0.8, 0.2))
  knots
  
  #Separate retained and removed data
  data.fit <- data[data$hold==FALSE,]
  data.pred <- data[data$hold==TRUE,]
  
  mod <- brokenstick(y=data.fit$y,
                     x=data.fit$age,
                     subject=data.fit$id,
                     storeX=T,
                     knots=knots,
                     Boundary.knots = c(
                       min(data.fit$age, na.rm = TRUE),
                       max(data.fit$age, na.rm = TRUE)))
  mod

## ------------------------------------------------------------------------
  a <- data[data$id==UID[1],]
  b <- predict(mod, a$y, a$age)

  plot(x = a$age, y = a$y, type = "b")
  brk <- c(mod@knots, mod@Boundary.knots[2])
  brk
  lines(x = brk, y = b, col = "red", lwd = 2, lty = 2, type = "b")
  abline(v = brk, lty = 5, col = "grey80")

## ------------------------------------------------------------------------
  a <- data[data$id==UID[1],]
  b <- predict(mod, a$y, a$age, type = "response")

  plot(x = a$age, y = a$y, type = "b")
  points(x = a$age, y = b, col = "red", lwd = 2, lty = 2, type = "b")
  abline(v = a$age, lty = 5, col = "grey80")

## ----fig.height=7, fig.width=7-------------------------------------------
  ## predict for all data
  ds <- split(data, f = data$id)
  result <- vector("list", length(ds))

  for (i in 1:length(ds)) {
    d <- ds[[i]]
    if (nrow(d) > 0) result[[i]] <- predict(mod, d$y, d$age, type = "response")
  }

  data$zhat <- unlist(result)
  
  library("MASS")
  eqscplot(x = data$zhat, y = data$y, xlab = "Predicted Z-score", ylab = "Observed Z-score", main = "All children") 
  abline(0, 1, col = "blue")

## ----fig.height=7, fig.width=7-------------------------------------------
# convert Z-score to CM
library("AGD")   # get AGD 0.35 from CRAN 

data$yhat <- z2y(z = data$zhat, 
                 x = data$age,
                 sex = ifelse(data$sex == "female", "F", "M"),
                 ref = get("who.hgt", pos = "package:AGD"))

eqscplot(x = data$yhat, y = data$hgt, xlab = "Predicted (cm)", 
     ylab = "Observed (cm)", main = "All children")
abline(0, 1, col = "blue")

## ------------------------------------------------------------------------
library(lattice)
xyplot(hgt ~ yhat | hold, data = data, xlab = "Predicted (cm)", 
     ylab = "Observed (cm)", main = "All children", aspect = "iso")

