#' Weight loss self-measurement data
#'
#' Longitudinal weight measurements from 12 individuals with 63 daily
#' measurement under three conditions.
#'
#' @name weightloss
#' @docType data
#' @format
#' A \code{data.frame} with 695 rows and 6 columns:
#' \describe{
#' \item{subject}{ID, consecutive person number 1-12 (integer)}
#' \item{day}{Measurement day, 0-62 (integer)}
#' \item{sex}{Sex, 1 = male, 0 = female (integer)}
#' \item{week}{Week number, 1-9 (integer)}
#' \item{condition}{Experimental condition (integer)}
#' \item{body_weight}{Body weight in kg (numeric)}
#' }
#' @source Krone T, Boessen R, Bijlsma S, van Stokkum R, Clabbers NDS, Pasman WJ (2020).
#' The possibilities of the use of N-of-1 and do-it-yourself trials in nutritional research.
#' \emph{PloS ONE}, \bold{15}, 5, e0232680.
#' @keywords datasets
NULL
