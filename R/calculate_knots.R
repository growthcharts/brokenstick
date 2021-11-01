calculate_knots <- function(x, k, internal, boundary) {

  k_orig <- k
  knots_orig <- internal
  boundary_orig <- boundary

  # for NULL or length not 2, set boundary to data range
  if (length(boundary_orig) != 2L) {
    boundary <- range(x, na.rm = TRUE)
  }

  # make sure that boundary includes all knots
  if (!is.null(knots_orig)) {
    boundary[1L] <- min(min(knots_orig, na.rm = TRUE), boundary[1L])
    boundary[2L] <- max(max(knots_orig, na.rm = TRUE), boundary[2L])
  }

  if (length(knots_orig)) {
    # if there is vector input via knots
    # trim knots to exclude boundary points, and set k
    knots <- as.numeric(knots_orig)
    knots <- knots[knots > boundary[1L] & knots < boundary[2L]]
    k <- length(knots)
  } else {
    # no user knots, set knots from
    if (is.null(k_orig)) k_orig <- 0L
    if (k_orig >= 0L && k_orig <= 50L) {
      k <- k_orig
      knots <- quantile(x,
                        probs = seq(0, 1, length.out = k + 2L),
                        na.rm = TRUE)[-c(1L, k + 2L)]
    }
    else {
      stop("Number of internal knots `k` outside range 0-50")
    }

  }
  return(list(k = k, internal = knots, boundary = boundary))
}
