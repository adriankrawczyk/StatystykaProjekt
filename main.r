### Załadowanie wymaganych bibliotek
library(smoof)
library(ecr)
library(ggplot2)
library(kableExtra)
library(stats)

# Ustawienie ziarna dla reprodukowalności wyników
set.seed(12345)

### Funkcje pomocnicze 

source("ga.r")
source("point.r")

### Funkcje testowe

# Tworzenie funkcji testowych dla różnych wymiarów
create_test_functions <- function() {
  list(
    ackley = list(
      f2D = makeAckleyFunction(2),
      f10D = makeAckleyFunction(10),
      f20D = makeAckleyFunction(20),
      lower = -32.768,
      upper = 32.768
    ),
    alpine02 = list(
      f2D = makeAlpine02Function(2),
      f10D = makeAlpine02Function(10),
      f20D = makeAlpine02Function(20),
      lower = 0,
      upper = 10
    )
  )
}

### Eksperymenty

# Uruchomienie eksperymentów dla danej funkcji
run_experiment <- function(fn_info, dimensions, n_repeats = 100, n_evals = 1000) {
  # Przypisz funkcje i parametry
  fn <- switch(
    dimensions,
    "2D" = fn_info$f2D,
    "10D" = fn_info$f10D,
    "20D" = fn_info$f20D
  )
  dims <- as.integer(gsub("D", "", dimensions))
  
  # Uruchom PRS
  prs_results <- replicate(
    n_repeats,
    performPRS(n_evals, fn, dims, fn_info$lower, fn_info$upper)
  )
  
  # Uruchom GA
  ga_results <- performGA(
    n_repeats, n_evals, fn, dims, 
    fn_info$lower, fn_info$upper
  )
  
  list(prs = prs_results, ga = ga_results)
}

### Wizualizacja wyników
visualize_results <- function(prs, ga, fn_name, dimensions) {
  df <- data.frame(
    Algorithm = factor(rep(c("PRS", "GA"), each = length(prs))),
    Value = c(prs, ga)
  )
  
  # Histogramy
  p1 <- ggplot(df, aes(x = Value, fill = Algorithm)) +
    geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
    ggtitle(paste(fn_name, dimensions, "Rozkład wyników")) +
    theme_minimal()
  
  # Wykres pudełkowy
  p2 <- ggplot(df, aes(x = Algorithm, y = Value, fill = Algorithm)) +
    geom_boxplot() +
    ggtitle(paste(fn_name, dimensions, "Porównanie metod")) +
    theme_minimal()
  
  print(p1)
  print(p2)
}

### Analiza statystyczna
perform_analysis <- function(prs, ga, fn_name, dimensions) {
  test <- t.test(prs, ga)
  
  cat("\n", fn_name, dimensions, "\n")
  cat("Średnia PRS:", mean(prs), "\n")
  cat("Średnia GA:", mean(ga), "\n")
  cat("Różnica:", mean(prs) - mean(ga), "\n")
  cat("95% CI różnicy:", round(test$conf.int, 3), "\n")
  cat("p-value:", format.pval(test$p.vaqlue, digits = 3), "\n")
}

### Główna funkcja wykonująca cały eksperyment
main <- function() {
  # Tworzenie funkcji testowych
  test_functions <- create_test_functions()
  
  # Przeprowadzenie eksperymentów dla wszystkich konfiguracji
  for (fn_name in names(test_functions)) {
    for (dims in c("2D", "10D", "20D")) {
      results <- run_experiment(test_functions[[fn_name]], dims)
      
      # Wizualizacja
      visualize_results(results$prs, results$ga, fn_name, dims)
      
      # Analiza statystyczna
      perform_analysis(results$prs, results$ga, fn_name, dims)
    }
  }
}

### Uruchomienie głównego programu
main()