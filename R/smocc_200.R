#' Infant growth of 0-2 years, SMOCC data extract
#'
#' Longitudinal height and weight measurements during ages 0-2 years
#' for a representative sample of 1933 Dutch children born in 1988-1989.
#' The dataset \code{smocc_200} is a subset of the full data covering
#' 200 children.
#'
#' @name smocc_200
#' @docType data
#' @format
#' A tibble with 1940 rows and 7 columns:
#' \describe{
#' \item{subjid}{ID, unique \code{id} of each child (numeric)}
#' \item{age}{Decimal age, 0-2.12 years (numeric)}
#' \item{sex}{Sex, \code{"male"} or \code{"female"} (character)}
#' \item{ga}{Gestational age, completed weeks (numeric)}
#' \item{bw}{Birth weight in grammes (numeric)}
#' \item{hgt}{Height measurement in cm (34-102) (numeric)}
#' \item{hgt.z}{Height in SDS relative to WHO standard) (numeric)}
#' }
#' @source Herngreen WP, van Buuren S, van Wieringen JC, Reerink JD, Verloove-Vanhorick SP & Ruys JH (1994). Growth in length and weight from birth to 2 years of a representative sample of Netherlands children (born in 1988-89) related to socio-economic status and other background characteristics. \emph{Annals of Human Biology}, \bold{21}, 449-463.
#' @keywords datasets
NULL

#' Broken stick model fitted on 200 children
#'
#' The object \code{fit_200} is an object of class \code{brokenstick} that contains the fitted broken stick model. The class \code{brokenstick} extend the \code{lmerMod} class with additional attributes.
#'
#' @name fit_200
#' @docType data
#' @format An object of class \code{brokenstick}, fitted by the
#' \code{brokenstick()}. This is an \code{lmerMod} object with additional
#' slots:
#' \describe{
#' 	\item{knots}{A vector of internal knots used by \code{make_basis()}}
#' 	\item{boundary}{The boundary knots of the linear spline}
#' 	\item{degree}{The degree of the spline. The brokenstick model corresponds to \code{degree = 1}.}
#' 	\item{bs.call}{An object of class \code{call} representing the call that was used to construct the object.}
#' 	\item{xy}{A data frame with column names \code{subjid}, \code{x} and \code{y} containing the data on which the model was fitted when called by the call in slot \code{bs.call}.}
#' 	}
#'
#' @seealso \code{\link[lme4]{lmer}}, \code{\link[lme4]{merMod-class}},
#' \code{\link{brokenstick}}
#' @keywords datasets
NULL
