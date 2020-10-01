#' Weight loss self-measurement data
#'
#' Longitudinal weight measurements from 12 individuals with 63 daily
#' measurement under three conditions.
#' @note Constructed from file `pone.0232680.s001.csv`. We renumbered `subject`
#' to consecutive integers 1-2 (as in the paper), corrected an error in the
#' `condition` variable for subjects 4 and 12 to match the paper's Figure 4,
#' and filtered the records to the ones woth an observed `body_weight` variable.
#' @name weightloss
#' @docType data
#' @format
#' A \code{data.frame} with 695 rows and 6 columns:
#' \describe{
#' \item{subject}{ID, consecutive person number 1-12 (integer)}
#' \item{day}{Measurement day, 0-62 (integer)}
#' \item{sex}{Sex, 1 = male, 0 = female (integer)}
#' \item{week}{Week number, 1-9 (integer)}
#' \item{condition}{Condition (control, diet, activity) (factor)}
#' \item{body_weight}{Body weight in kg (numeric)}
#' }
#' @source Krone T, Boessen R, Bijlsma S, van Stokkum R, Clabbers NDS, Pasman WJ (2020).
#' The possibilities of the use of N-of-1 and do-it-yourself trials in nutritional research.
#' \emph{PloS ONE}, \bold{15}, 5, e0232680.
#' @keywords datasets
NULL
