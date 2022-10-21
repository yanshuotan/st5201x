# Tutorial 9 Question 1 ---------------------------------------------------

set.seed(42)
theta <- 1
nsamp <- 50
X <- runif(nsamp)
nonparametric_boot <- # FILL IN
parametric_boot <- vector("numeric", 1000)
for (b in 1:1000) {
  parametric_boot[[b]] <- # FILL IN
}
true_density <- function(x, nsamp = 50) {
  # FILL IN
}
grid <- seq(0, 1, length.out = 100)
plot_df <- tibble(x = grid, 
                  true = true_density(grid),
                  parametric = ecdf(parametric_boot)(grid),
                  nonparametric = ecdf(nonparametric_boot)(grid)) %>%
  pivot_longer(cols = c("true", "parametric", "nonparametric"), 
               names_to = "type", values_to = "y")
plot_df <- plot_df %>% mutate(type = factor(type))
ggplot(plot_df) + geom_line(aes(x = x, y = y, color = type))


# Tutorial 9 Question 4 ---------------------------------------------------

T_stat <- function(X) {
  # Function to compute skewness
  # FILL IN
}

true_skewness <- (exp(9/2) - 3 * exp(5/2) + 2 * exp(3/2)) /
  (exp(2) - exp(1)) ** (3/2)

make_ci_df <- function(x, B = 1000, func, alpha = 0.05) {
  # Make the df with the results for the 4 types of bootstrap CIs
  CIs <- matrix(0, nrow = 4, ncol = 2)
  ci_funcs <-  c(normal_interval, percentile_interval, pivotal_interval, 
                 studentized_pivotal_interval)
  for (i in 1:4) {
    CIs[i, ] <- ci_funcs[[i]](x = x, B = B, func = func, alpha = alpha)
  }
  CI_df <- tibble(lower = CIs[, 1], upper = CIs[, 2])
  CI_df[["type"]] <- c("Normal interval", "Percentile interval",
                       "Pivotal interval", "Studentized pivotal interval")
  CI_df %>% select(type, everything())
}

# WARNING: The following code may take a while to run. You may set B = 100 if 
# it takes too long with B = 1000.
coverage_counts <- rep(0, 4)
nreps <- 100
for (i in 1:nreps) {
  nsamp <- 50
  Y <- rnorm(nsamp)
  X <- exp(Y)
  CI_df <- make_ci_df( # FILL IN)
  coverage_counts = coverage_counts + # FILL IN
}
coverage_counts = coverage_counts / nreps


# Tutorial 9 Question 5 ---------------------------------------------------

bioequivalence_df <- 
  tibble(subject = 1:8,
         placebo = c(9243, 9671, 11792, 13357, 9055, 6290, 12412, 18806),
         old = c(17649, 12013, 19979, 21816, 13850, 9806, 17208, 29044),
         new = c(16649, 14614, 17274, 23798, 12560, 10157, 16570, 26325))

effect_diff_ratio <- function(x) {
  x <- as.matrix(x)
  # FILL IN
}

bioeq_boot <- # FILL IN
ggplot(data = tibble(x = bioeq_boot)) + # FILL IN
