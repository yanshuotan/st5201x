
# Question 3 --------------------------------------------------------------

distance_to_stationary <- function(mu0, P, pi, t) {
  mut <- # FILL IN
  sqrt(sum((mut - pi) ** 2))
}

# Question 4 --------------------------------------------------------------

# c)

trajectory <- sample_trajectory(...) # FILL IN
require(stats)
acf_results <- acf(trajectory, plot = FALSE)

# d)

rho <- function(L, p = 0.25) {
  # FILL IN
}

rho_vals <- rep(0, 5)
for (i in 1:5) {
  rho_vals[[i]] <- rho(i) # Use theoretical value not ACF estimate
}

ESS <- T / (1 + 2 * sum(rho_vals))

# e)

T <- 200
M <- 500
trajectories <- matrix(0, M, T+1)
for (m in 1:M) {
  x0 <- sample(1:2, 1)
  trajectories[m, ] <- sample_trajectory(...) # FILL IN
}
h_trajectories <- trajectories * 2 - 1
h_means <- apply(h_trajectories, 1, mean)
var(h_means)

# Question 5 --------------------------------------------------------------

# f)
target <- c(0.4, 0.2, 0.2, 0.1, 0.1)
T <- 200
M <- 500
trajectories <- matrix(0, M, T+1)
for (m in 1:M) {
  trajectories[m, ] <- sample_mh_trajectory(...) # FILL IN
}

tmp <- apply(trajectories[, 101:200], 1, table) / 100
means <- apply(tmp, 1, mean)
sds <- apply(tmp, 1, ...) # FILL IN

plot_df <- as_tibble(t(tmp)) %>% pivot_longer(c(1, 2, 3, 4, 5), 
                                              names_to = "state", 
                                              values_to = "freq")

ggplot(data = plot_df) + geom_boxplot(aes(x = state, y = freq))


# Question 6 --------------------------------------------------------------

log_mvt_beta_density <- function(x, alpha = 3, beta = 3) {
  sum(dbeta(x, shape1 = alpha, shape2 = beta, log = TRUE))
}

d <- 5

results <- rwmetrop0(..., x0 = rep(0.5, d), ...) # FILL IN
mcmc_results <- mcmc(results$par)

traceplot(mcmc_results)
autocorr.plot(mcmc_results)
summar(mcmc_results)
