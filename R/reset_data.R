reset_data <- function(data, names, x = NULL, y = NULL, group = NULL) {

  if (is.null(x) && is.null(y) && is.null(group)) {
    if (is.null(data)) stop("Expected argument `new_data` not found.")
    return(data)
  }

  # 1 create x for every group in data
  if (!is.null(x) && is.null(y) && is.null(group)) {
    if (is.null(data))
      stop("Expected argument `new_data` not found.", call. = FALSE)
    reset <- expand.grid(
      x = x,
      y = NA,
      g = unique(data[[names$g]])
    )
    colnames(reset) <- c(names$x, names$y, names$g)
    reset <- bind_rows(data = data, added = reset, .id = ".source")
    # message("Reset new_data: predict at `x` in every group.")
  }

  # 2 create new group with x and y
  if (!is.null(x) && !is.null(y) && is.null(group)) {
    if (length(x) != length(y))
      stop("Incompatible length of `x` and `y`.", call. = FALSE)
    reset <- data.frame(
      s = "added",
      x = x,
      y = y,
      g = 0)
    colnames(reset) <- c(".source", names$x, names$y, names$g)
    # message("Reset new_data: new group with `x` and `y`.")
  }

  # 3 subset groups from data
  if (is.null(x) && is.null(y) && !is.null(group)) {
    if (is.null(data))
      stop("Expected argument `new_data` not found.", call. = FALSE)
    reset <- data[data[[names$g]] %in% group, , drop = FALSE]
    reset <- bind_cols(.source = "data", reset)
    # message("Reset new_data: subset of groups.")
  }

  # 4 create x for subset of groups from data
  if (!is.null(x) && is.null(y) && !is.null(group)) {
    if (is.null(data))
      stop("Expected argument `new_data` not found.", call. = FALSE)
    groups <- intersect(data[[names$g]], group)
    reset <- expand.grid(
      s = "added",
      x = x,
      y = NA,
      g = groups
    )
    colnames(reset) <- c(".source", names$x, names$y, names$g)
    reset <- bind_rows(data = data[data[[names$g]] %in% groups, , drop = FALSE],
                       added = reset, .id = ".source") %>%
      relocate(".source")
    # message("Reset new_data: predict at `x` in subset of groups.")
  }

  # 5 create data.frame from vectors x, y and group
  if (!is.null(x) && !is.null(y) && !is.null(group)) {
    if (length(x) != length(y))
      stop("Incompatible length of `x` and `y`.", call. = FALSE)
    if (length(x) != length(group))
      stop("Incompatible length of `x` and `group`.", call. = FALSE)
    groups <- intersect(data[[names$g]], group)
    reset <- data.frame(
      x = x,
      y = y,
      g = group)
    colnames(reset) <- c(names$x, names$y, names$g)
    reset <- bind_rows(data = data[data[[names$g]] %in% groups, , drop = FALSE],
                       added = reset, .id = ".source") %>%
      relocate(".source")
    # message("Reset new_data: predict from vectors `x`, `y` and `group`.")
  }

  reset
}
