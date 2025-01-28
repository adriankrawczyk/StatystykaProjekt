# prs.R
generate_random_points <- function(n, dimensions, lower, upper) {
  matrix(
    runif(n * dimensions, min = lower, max = upper),
    ncol = dimensions
  )
}

prs_search <- function(fn, points) {
  min(apply(points, 1, fn))
}