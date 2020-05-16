calculate_knots <- function(x, k, knots, boundary) {

  k_orig <- k
  knots_orig <- knots
  boundary_orig <- boundary

  # if not specified, define boundary as data range
  range <- range(x, na.rm = TRUE)
  if (is.null(boundary_orig)) boundary <- range
  if (length(boundary_orig) != 2) boundary <- range

  # if knots is specified
  #   extend lower boundary to min(knots)
  #   extend upper boundary to max(knots)
  if (!is.null(knots_orig)) {
    boundary[1] <- min(min(knots_orig, na.rm = TRUE), boundary[1])
    boundary[2] <- max(max(knots_orig, na.rm = TRUE), boundary[2])
  }

  # set k to zero if not specified
  if (is.null(k_orig)) k <- 0

  # if there is vector input via knots and if no k specified
  # trim knots to exclude boundary points, and calculate k
  if (is.null(k_orig) & length(knots_orig) >= 1) {
    knots <- as.numeric(knots_orig)
    knots <- knots[knots > boundary[1] & knots < boundary[2]]
    k <- length(knots)
  }

  # for scalar k, calculate equidense quantiles from the data
  if (!is.null(k_orig)) {
    if (k_orig >= 0 & k_orig <= 25) {
      k <- k_orig
      knots <- quantile(x,
                        probs = seq(0, 1, length.out = k + 2),
                        na.rm = TRUE
      )[-c(1, k + 2)]
    }
    else {
      stop("Number of knots outside range 0-25")
    }

  }
  return(list(k = k, knots = knots, boundary = boundary))
}