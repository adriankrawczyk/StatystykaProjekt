# Załadowanie wymaganych bibliotek
library(smoof)
library(ecr)
library(ggplot2)
library(kableExtra)
library(stats)
library(dplyr)
library(gridExtra)

# Ustawienie ziarna dla reprodukowalności wyników
set.seed(12345)

# Funkcje pomocnicze ------------------------------------------------------
getRandomPoint <- function(dimensions, lower, upper) {
  runif(dimensions, min = lower, max = upper)
}

performPRS <- function(numberOfEvals, givenFunc, dimensions, lower, upper) {
  points <- replicate(numberOfEvals, getRandomPoint(dimensions, lower, upper))
  values <- apply(points, 2, givenFunc)
  min(values)
}

performGA <- function(repeats, numberOfEvals, givenFunc, dimensions, lower, upper) {
  maxEvals <- list(stopOnEvals(numberOfEvals))
  
  lower_bounds <- rep(lower, dimensions)
  upper_bounds <- rep(upper, dimensions)
  
  result <- replicate(
    repeats,
    ecr(
      fitness.fun = givenFunc,
      n.dim = dimensions,
      lower = lower_bounds,
      upper = upper_bounds,
      minimize = TRUE,
      representation = "float",
      mu = 50L,
      lambda = 25L,
      terminators = maxEvals,
      mutator = setup(mutGauss, lower = lower_bounds, upper = upper_bounds)
    )$best.y
  )
  return(result)
}

# Funkcje testowe ---------------------------------------------------------
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

# Eksperymenty ------------------------------------------------------------
run_experiment <- function(fn_info, dimensions, n_repeats = 100, n_evals = 1000) {
  fn <- switch(
    dimensions,
    "2D" = fn_info$f2D,
    "10D" = fn_info$f10D,
    "20D" = fn_info$f20D
  )
  dims <- as.integer(gsub("D", "", dimensions))
  
  prs_results <- replicate(n_repeats, performPRS(n_evals, fn, dims, fn_info$lower, fn_info$upper))
  ga_results <- performGA(n_repeats, n_evals, fn, dims, fn_info$lower, fn_info$upper)
  
  list(prs = prs_results, ga = ga_results)
}

# Generowanie statystyk ---------------------------------------------------
generate_stats <- function(results, algorithm_name) {
  data.frame(
    Algorytm = algorithm_name,
    Średnia = mean(results),
    Minimum = min(results),
    Maksimum = max(results),
    Mediana = median(results),
    Dolny_kwartyl = quantile(results, 0.25),
    Górny_kwartyl = quantile(results, 0.75)
  )
}

# Funkcje wizualizacji --------------------------------------------------
create_histogram <- function(data, algorithm_name, fn_name, dimensions) {
  mean_val <- mean(data)
  min_val <- min(data)
  
  ggplot(data.frame(Values = data), aes(x = Values)) +
    geom_histogram(fill = "lightblue", color = "black", bins = 30) +
    geom_vline(aes(xintercept = mean_val, color = "Mean"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = min_val, color = "Minimum"), linetype = "dashed", size = 1) +
    scale_color_manual(name = "Legend", 
                      values = c("Mean" = "red", "Minimum" = "blue")) +
    labs(title = paste(fn_name, dimensions, algorithm_name),
         x = "Values",
         y = "Count") +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

create_boxplots <- function(prs, ga, fn_name, dimensions) {
  data <- data.frame(
    Value = c(prs, ga),
    Algorithm = factor(rep(c("PRS", "GA"), each = length(prs)))
  )
  
  ggplot(data, aes(x = Algorithm, y = Value, fill = Algorithm)) +
    geom_boxplot() +
    labs(title = paste(fn_name, dimensions),
         y = "Znalezione minima funkcji",
         x = "Algorithm") +
    theme_minimal() +
    scale_fill_manual(values = c("PRS" = "#F8766D", "GA" = "#00BFC4")) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    )
}

# Wizualizacja wyników --------------------------------------------------
visualize_results <- function(prs, ga, fn_name, dimensions) {
  # Statystyki
  df <- rbind(
    generate_stats(prs, "PRS"),
    generate_stats(ga, "GA")
  )
  
  # Wyświetlenie statystyk w konsoli
  cat("\n=== Statystyki dla", fn_name, dimensions, "===\n")
  print(df)
  
  # Tworzenie histogramów
  hist_prs <- create_histogram(prs, "PRS", fn_name, dimensions)
  hist_ga <- create_histogram(ga, "GA", fn_name, dimensions)
  
  # Tworzenie boxplota
  boxplot <- create_boxplots(prs, ga, fn_name, dimensions)
  
  # Zapisywanie wykresów
  ggsave(paste0(fn_name, "_", dimensions, "_hist_prs.png"), hist_prs, width = 8, height = 6)
  ggsave(paste0(fn_name, "_", dimensions, "_hist_ga.png"), hist_ga, width = 8, height = 6)
  ggsave(paste0(fn_name, "_", dimensions, "_boxplot.png"), boxplot, width = 8, height = 6)
  
  # Wyświetlanie wykresów
  print(hist_prs)
  print(hist_ga)
  print(boxplot)
}

# Analiza statystyczna --------------------------------------------------
perform_analysis <- function(prs, ga, fn_name, dimensions) {
  test <- t.test(prs, ga, paired = TRUE)
  
  cat("\n=== Analiza statystyczna dla", fn_name, dimensions, "===")
  cat("\nRóżnica średnich:", test$estimate)
  cat("\n95% CI:", paste(round(test$conf.int, 4), collapse = " do "))
  cat("\np-value:", test$p.value, "\n")
}

# Funkcja do utworzenia zbiorczego wykresu pudełkowego ------------------
create_combined_boxplot <- function(results_list) {
  # Przygotowanie danych
  all_data <- data.frame()
  
  for (fn_name in names(results_list)) {
    for (dim in c("2D", "10D", "20D")) {
      data <- results_list[[fn_name]][[dim]]
      temp_df <- data.frame(
        Value = c(data$prs, data$ga),
        Algorithm = rep(c("PRS", "GA"), each = length(data$prs)),
        Function = fn_name,
        Dimension = dim
      )
      all_data <- rbind(all_data, temp_df)
    }
  }
  
  # Tworzenie wykresu
  p <- ggplot(all_data, aes(x = Algorithm, y = Value, fill = Algorithm)) +
    geom_boxplot() +
    facet_grid(Function ~ Dimension, scales = "free_y") +
    scale_fill_manual(values = c("PRS" = "#F8766D", "GA" = "#00BFC4")) +
    labs(title = "Porównanie algorytmów PRS i GA dla wszystkich funkcji i wymiarów",
         y = "Znalezione minima funkcji") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  # Zapisanie wykresu
  ggsave("combined_boxplot.png", p, width = 12, height = 8)
  
  # Wyświetlenie wykresu
  print(p)
}

# Główna funkcja --------------------------------------------------------
main <- function() {
  test_functions <- create_test_functions()
  all_results <- list()
  
  for (fn_name in names(test_functions)) {
    cat("\n\n=== Rozpoczęcie analizy dla funkcji", fn_name, "===\n")
    all_results[[fn_name]] <- list()
    
    for (dims in c("2D", "10D", "20D")) {
      cat("\n--- Wymiar:", dims, "---\n")
      results <- run_experiment(test_functions[[fn_name]], dims)
      all_results[[fn_name]][[dims]] <- results
      visualize_results(results$prs, results$ga, fn_name, dims)
      perform_analysis(results$prs, results$ga, fn_name, dims)
    }
  }
  
  # Tworzenie zbiorczego wykresu pudełkowego
  create_combined_boxplot(all_results)
}

# Uruchomienie analizy
main()