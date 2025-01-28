# ms.R
calculate_ms <- function(n_starts, dimensions, lower, upper, objective_fn) {
  # Generowanie punktów startowych
  start_points <- matrix(
    runif(n_starts * dimensions, min = lower, max = upper),
    ncol = dimensions
  )
  
  # Uruchamianie optymalizacji lokalnej
  results <- apply(start_points, 1, function(point) {
    optim(
      par = point,
      fn = objective_fn,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper,
      control = list(maxit = 1000)
    )
  })
  
  # Znajdowanie najlepszego wyniku
  values <- sapply(results, function(res) res$value)
  counts <- sapply(results, function(res) res$counts[["function"]])
  
  c(
    min_value = min(values),
    eval_count = sum(counts)
  )
}