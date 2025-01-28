# ga.R
ga_search <- function(fn, dimensions, lower, upper, budget) {
  # Konfiguracja algorytmu genetycznego
  lower_bounds <- rep(lower, dimensions)
  upper_bounds <- rep(upper, dimensions)
  
  # Uruchomienie algorytmu genetycznego
  result <- ecr(
    fitness.fun = fn,
    n.dim = dimensions,
    lower = lower_bounds,
    upper = upper_bounds,
    minimize = TRUE,
    representation = "float",
    mu = 50L,  # Liczba rodziców
    lambda = 25L,  # Liczba potomków
    terminators = list(stopOnIters(budget)),  # Warunek zatrzymania
    mutator = setup(mutGauss, lower = lower_bounds, upper = upper_bounds)
  )
  
  # Zwrócenie najlepszego wyniku
  result$best.y
}