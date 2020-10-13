#' Infant growth of 0-2 years, SMOCC data extract
#'
#' Longitudinal height and weight measurements during ages 0-2 years for a
#' representative sample of 1933 Dutch children born in 1988-1989. The dataset
#' \code{smocc_200} is a subset of the full data covering 200 children.
#'
#' @name smocc_200
#' @docType data
#' @format A tibble with 1940 rows and 7 columns: \describe{ \item{id}{ID,
#' unique \code{id} of each child (numeric)} \item{age}{Decimal age, 0-2.12
#' years (numeric)} \item{sex}{Sex, \code{"male"} or \code{"female"}
#' (character)} \item{ga}{Gestational age, completed weeks (numeric)}
#' \item{bw}{Birth weight in grammes (numeric)} \item{hgt}{Height measurement in
#' cm (34-102) (numeric)} \item{hgt.z}{Height in SDS relative Fourth Dutch
#' Growth Study 1997 (numeric)} }
#' @source Herngreen WP, van Buuren S, van Wieringen JC, Reerink JD,
#'   Verloove-Vanhorick SP & Ruys JH (1994). Growth in length and weight from
#'   birth to 2 years of a representative sample of Netherlands children (born
#'   in 1988-89) related to socio-economic status and other background
#'   characteristics. \emph{Annals of Human Biology}, \bold{21}, 449-463.
#' @keywords datasets
NULL

#' Broken stick model with nine lines for 200 children
#'
#' The object `fit_200` is an object of class `brokenstick` that contains the
#' fitted broken stick model.
#'
#' @name fit_200
#' @docType data
#' @format An object of class [brokenstick][brokenstick-class], fitted by the
#'   [brokenstick()].
#'
#' @details
#' This dataset was constructed as
#' ```{r eval=FALSE}
#' knots <- round(c(0, 1, 2, 3, 6, 9, 12, 15, 18, 24)/12, 4)
#' fit_200 <- brokenstick(hgt.z ~ age | id, data = smocc_200,
#'                        knots = knots, boundary = c(0, 3))
#' ```
#' @keywords datasets
NULL
