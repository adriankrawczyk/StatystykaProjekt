# Generowanie losowego punktu w dziedzinie
getRandomPoint <- function(dimensions, lower, upper) {
  runif(dimensions, min = lower, max = upper)
}
