#' @importFrom coda            mcmc
#' @importFrom dplyr           bind_rows bind_cols filter group_by
#'                             mutate pull relocate select
#'                             starts_with ungroup `%>%`
#' @importFrom lme4            fixef lmer lmerControl ngrps VarCorr
#' @importFrom methods         slot
#' @importFrom matrixsampling  rwishart
#' @importFrom rlang           .data arg_match
#' @importFrom splines         bs
#' @importFrom stats           approx as.formula coef cor cov2cor
#'                             fitted model.frame lm
#'                             model.matrix na.exclude na.omit
#'                             na.pass optim
#'                             predict quantile rgamma rnorm
#'                             rWishart setNames smooth var
#' @importFrom tidyr           drop_na pivot_longer pivot_wider
#' @importFrom utils           askYesNo install.packages
NULL