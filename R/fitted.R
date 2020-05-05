#' Calculate fitted values
#'
#' @aliases fitted.brokenstick
#' @param object An object of class \code{brokenstick}
#' @param predict_NA A logical indicating whether the rows with a missing \code{y} value should be predicted. The default is \code{predict_NA = TRUE}.
#' @param \dots Additional arguments passed down to \code{predict.merMod()}.
#' @return A vector of predicted values.
#' @author Stef van Buuren 2016
#' @note   If \code{x} is known and \code{y} is \code{NA} then \code{lme4:::fitted.medMod()} returns \code{NA}. The \code{fitted.brokenstick()} function intercepts subjects with \code{NA}'s in \code{y}, and calculates \code{yhat} for the relevent rows by calling \code{predict.brokenstick_export()}.
#' @family brokenstick
#' @export
fitted.brokenstick <- function(object, predict_NA = TRUE, ...) {
  # deal with standard case through lme4
  class(object) <- "merMod"
  yhat_lme4 <- fitted(object, ...)
  if (!(predict_NA)) return(yhat_lme4)
  if (!any(is.na(yhat_lme4))) return(yhat_lme4)

  # prepare for re-prediction
  class(object) <- "brokenstick"
  xy <- cbind(get_xy(object), lme4 = yhat_lme4)
  if (!any(is.na(xy$y))) return(yhat_lme4)

  # reevaluate cases with missing y
  exp <- export(object)
  ds <- split(xy, f = xy$subjid, drop = TRUE)
  result <- vector("list", length(ds))
  for (i in seq_along(ds)) {
    d <- ds[[i]]
    result[[i]] <- d$lme4
    if (any(is.na(d$y)) && nrow(d) > 0)
      result[[i]] <- predict(exp, y = d$y, x = d$x, subjid = d$subjid[1],
                             output = "vector")
  }
  return(unlist(result))
}