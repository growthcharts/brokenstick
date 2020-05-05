#'Infant growth of 0-2 years, SMOCC data extract
#'
#'Longitudinal height and weight measurements during ages 0-2 years for a representative sample of 1933 Dutch children born in 1988-1989.
#'The dataset \code{smocc.hgtwgt} is a subset of the full data covering the first 206 children.
#'
#'@name smocc.hgtwgt
#'@docType data
#'@format
#'A data frame with 2000 rows and 12 columns:
#'\describe{
#'\item{src}{Source, here \code{"smocc"} (factor)}
#'\item{id}{ID, unique \code{id} of each child (factor)}
#'\item{rec}{Record number, consecutive 1-11 (numeric)}
#'\item{nrec}{Number of child records, 6-11 (numeric)}
#'\item{age}{Decimal age, 0-2.99 (numeric)}
#'\item{sex}{Sex, \code{"male"} or \code{"female"} (factor)}
#'\item{etn}{Etnicity, \code{"MA"}, \code{"NL"}, or \code{"TU"} (factor)}
#'\item{ga}{Gestational age in completed weeks (25-44) (numeric)}
#'\item{bw}{Birth weight in grammes (810-5100) (numeric)}
#'\item{hgt}{Height measurement in cm (34-102) (numeric)}
#'\item{wgt}{Weight measurement in kg (0.8-20.5) (numeric)}
#'\item{hgt.z}{Height in SDS relative to WHO standard) (numeric)}
#'}
#'@source Herngreen WP, van Buuren S, van Wieringen JC, Reerink JD, Verloove-Vanhorick SP & Ruys JH (1994). Growth in length and weight from birth to 2 years of a representative sample of Netherlands children (born in 1988-89) related to socio-economic status and other background characteristics. \emph{Annals of Human Biology}, \bold{21}, 449-463.
#'@keywords datasets
NULL

#'Infant growth of 0-2 years, SMOCC data extract
#'
#'Longitudinal height and weight measurements during ages 0-2 years for a representative sample of 1933 Dutch children born in 1988-1989.
#'The dataset \code{smocc_hgtwgt} is a subset of the full data covering the first 206 children.
#'
#'@name smocc_hgtwgt
#'@docType data
#'@format
#'A data frame with 2000 rows and 14 columns:
#'\describe{
#'\item{src}{Source, here \code{"smocc"} (character)}
#'\item{subjid}{ID, unique identifyer of each child (character)}
#'\item{rec}{Record number, consecutive 1-11 (numeric)}
#'\item{nrec}{Number of child records, 6-11 (numeric)}
#'\item{age}{Decimal age, 0-2.99 (numeric)}
#'\item{agedays}{Age in days (numeric)}
#'\item{sex}{Sex, \code{"Male"} or \code{"Female"} (character)}
#'\item{etn}{Etnicity, \code{"MA"}, \code{"NL"}, or \code{"TU"} (factor)}
#'\item{gagebrth}{Gestational age at birth (days) (numeric)}
#'\item{birthwt}{Birth weight in grammes (810-5100) (numeric)}
#'\item{htcm}{Length/height in cm (34-102) (numeric)}
#'\item{wtkg}{Weight measurement in kg (0.8-20.5) (numeric)}
#'\item{haz}{Height in SDS relative to WHO child growth standard (numeric)}
#'\item{waz}{Weight in SDS relative to WHO child growth standard (numeric)}
#'}
#'@source Herngreen WP, van Buuren S, van Wieringen JC, Reerink JD, Verloove-Vanhorick SP & Ruys JH (1994). Growth in length and weight from birth to 2 years of a representative sample of Netherlands children (born in 1988-89) related to socio-economic status and other background characteristics. \emph{Annals of Human Biology}, \bold{21}, 449-463.
#'@note This dataset contains the same information as
#'\code{\link{smocc.hgtwgt}}, but conforms HBGDki conventions.
#'@keywords datasets
NULL

#'Broken stick model fitted on 206 children
#'
#'The object \code{fit_206} is an object of class \code{brokenstick} that contains the fitted broken stick model. The class \code{brokenstick} extend the \code{lmerMod} class with additional attributes.
#'
#'@name fit_206
#'@docType data
#'@format An object of class \code{brokenstick}, fitted by the
#'\code{brokenstick()}. This is an \code{lmerMod} object with additional
#'slots:
#'\describe{
#'	\item{knots}{A vector of internal knots used by \code{make_basis()}}
#'	\item{boundary}{The boundary knots of the linear spline}
#'	\item{degree}{The degree of the spline. The brokenstick model corresponds to \code{degree = 1}.}
#'	\item{bs.call}{An object of class \code{call} representing the call that was used to construct the object.}
#'	\item{xy}{A data frame with column names \code{subjid}, \code{x} and \code{y} containing the data on which the model was fitted when called by the call in slot \code{bs.call}.}
#'	}
#'
#' @seealso \code{\link[lme4]{lmer}}, \code{\link[lme4]{merMod-class}},
#' \code{\link{brokenstick}}
#' @keywords datasets
NULL
