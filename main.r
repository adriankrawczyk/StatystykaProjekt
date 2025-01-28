# main.R
library(stats)
library(smoof)

# Źródłowe pliki z implementacjami algorytmów
source("prs.R")
source("ms.R")

# Funkcja pomocnicza do porównywania metod
compare_methods <- function(fn, dimensions, bounds, fn_name) {
  cat(paste("\nAnaliza funkcji", fn_name, "w", dimensions, "wymiarach:\n"))
  
  # Uruchomienie metody Multi-Start
  ms_results <- replicate(100, calculate_ms(
    n_starts = 100, 
    dimensions = dimensions, 
    lower = bounds[1], 
    upper = bounds[2], 
    objective_fn = fn
  ))
  
  # Wyodrębnienie wyników
  ms_values <- ms_results[1, ]
  eval_counts <- ms_results[2, ]
  
  # Obliczenie średniego budżetu obliczeniowego
  avg_budget <- round(mean(eval_counts))
  
  cat("\nMetoda Multi-Start:\n")
  cat(paste("Średni wynik:", round(mean(ms_values), 4), "\n"))
  cat(paste("Średni budżet obliczeniowy:", avg_budget, "\n"))
  
  # Uruchomienie PRS z porównywalnym budżetem
  prs_values <- replicate(100, prs_search(
    fn = fn, 
    points = generate_random_points(
      n = avg_budget,
      dimensions = dimensions,
      lower = bounds[1],
      upper = bounds[2]
    )
  ))
  
  cat("\nMetoda PRS:\n")
  cat(paste("Średni wynik:", round(mean(prs_values), 4), "\n"))
  
  # Zwrócenie wyników
  list(
    ms_values = ms_values,
    prs_values = prs_values,
    budget = avg_budget
  )
}

# Parametry testów
set.seed(12345)

# Funkcja Ackleya
ackley_bounds <- c(-32.768, 32.768)

# Funkcja Alpine02
alpine02_bounds <- c(0, 10)

# Przeprowadzenie eksperymentów
results <- list()

# Testowanie dla funkcji Ackley
cat("\n===== FUNKCJA ACKLEYA =====\n")
results$ackley_2d <- compare_methods(
  fn = makeAckleyFunction(dimensions = 2),
  dimensions = 2,
  bounds = ackley_bounds,
  fn_name = "Ackley"
)

results$ackley_10d <- compare_methods(
  fn = makeAckleyFunction(dimensions = 10),
  dimensions = 10,
  bounds = ackley_bounds,
  fn_name = "Ackley"
)

results$ackley_20d <- compare_methods(
  fn = makeAckleyFunction(dimensions = 20),
  dimensions = 20,
  bounds = ackley_bounds,
  fn_name = "Ackley"
)

# Testowanie dla funkcji Alpine02
cat("\n===== FUNKCJA ALPINE02 =====\n")
results$alpine02_2d <- compare_methods(
  fn = makeAlpine02Function(dimensions = 2),
  dimensions = 2,
  bounds = alpine02_bounds,
  fn_name = "Alpine02"
)

results$alpine02_10d <- compare_methods(
  fn = makeAlpine02Function(dimensions = 10),
  dimensions = 10,
  bounds = alpine02_bounds,
  fn_name = "Alpine02"
)

results$alpine02_20d <- compare_methods(
  fn = makeAlpine02Function(dimensions = 20),
  dimensions = 20,
  bounds = alpine02_bounds,
  fn_name = "Alpine02"
)

# Zapis wyników do plików
save_results <- function(results, prefix) {
  for (dim in c("2d", "10d", "20d")) {
    write.csv(
      x = results[[paste0(prefix, "_", dim)]]$ms_values,
      file = paste0("data/", prefix, "_", dim, "_ms.csv"),
      row.names = FALSE
    )
    write.csv(
      x = results[[paste0(prefix, "_", dim)]]$prs_values,
      file = paste0("data/", prefix, "_", dim, "_prs.csv"),
      row.names = FALSE
    )
  }
}

dir.create("data", showWarnings = FALSE)
save_results(results, "ackley")
save_results(results, "alpine02")

cat("\nEksperyment zakończony pomyślnie. Wyniki zapisano w folderze 'data'.\n")