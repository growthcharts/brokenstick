#' Parse formula for brokenstick model
#'
#' A bare bones formula parser to extract variables names from
#' formulas of `y ~ x | g`. It return the name of
#' the first variable mentioned in each formula component.
#' @param f formula object
#' @return A `list` with elements `x`, `y` and `g`.
#' Each element has length 1.
#' @author Stef van Buuren, 2020
#' @examples
#' \dontrun{
#' # examples that yield identical result
#' brokenstick:::parse_formula(y ~ x | z)
#' brokenstick:::parse_formula(y + a ~ x + x1 | z + b)
#' brokenstick:::parse_formula(y + a + log(b) ~ x + x1 * c | z + d)
#' }
parse_formula <- function(f) {

  if (!inherits(f, "formula"))
    stop(call. = FALSE, "Not a formula.")
  if (length(f[[3]]) != 3L)
    stop(call. = FALSE, "Can't find RHS expansion in formula.")
  if (f[[3]][[1]] != "|")
    stop(call. = FALSE, "Can't find `|` operator in formula.")

  # Just take first variables - no support for `+` and friends
  y_name <- all.vars(f[[2]], max.names = 1L)
  x_name <- all.vars(f[[3]][[2]], max.names = 1L)
  g_name <- all.vars(f[[3]][[3]], max.names = 1L)

  vec <- c(x_name, y_name, g_name)
  if (any(duplicated(vec)))
    stop(call. = FALSE, "Found duplicate names in formula.")
  if (any(vec == "."))
    stop(call. = FALSE, "No support for `.` in formula.")

  list(x = x_name, y = y_name, g = g_name)
}
