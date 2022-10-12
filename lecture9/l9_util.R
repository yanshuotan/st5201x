library(tidyverse)

reg_func <- function(x) {
  log(1+x) * sin(x)**2
}

make_data <- function(nsamples = 100, eps = 0.5) {
  set.seed(42)
  tibble(x = runif(nsamples, 0, 5), y = reg_func(x) + rnorm(nsamples, 0, eps))
}