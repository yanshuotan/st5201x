source("sampling_util.R")

# Tutorial 10 Question 1 --------------------------------------------------

theoretical_quantile_std <- function(p, alpha, beta) {
  quantile_var <- # FILL IN
  sqrt(quantile_var)
}

# Calculating the standard deviation for sample quantiles to verify the 
# theoretical value
nsamp <- # FILL IN
n_iter <- 1000
sample_quantile <- rep(0, n_iter)
for (b in 1:n_iter) {
  gamma_samples <- # FILL IN
  sample_quantile[[b]] <- # FILL IN
}
sqrt(var(sample_quantile))

# Tutorial 10 Question 2 --------------------------------------------------

# Part a)
truncated_normal_density <- function(x) {
  y <- dnorm(x)
  y[x < -1 | x > 1] <- 0
}

unif_proposal_density <- function(x) {
  # FILL IN
}

unif_proposal_generator <- function(S = 1) {
  runif(S, -2, 2)
}

M <- get_opt_M(...) # FILL IN

sampling_results <- rej_sampling_1d(S = 1000, ...) # FILL IN

# Part b)

c_hat <- # FILL IN

ggplot(data = tibble(x = sampling_results$samples)) + 
  geom_histogram(aes(x = x, y = ..density..), bins = 20) +
  stat_function(data = tibble(x = c(-1, 1)), 
                fun = function(x) {1 / c_hat * truncated_normal_density(x)})

# Part c)

t_proposal_density <- function(x) {
  # FILL IN
}

t_proposal_generator <- function(S = 1) {
  rt(S, 3, 0) # The t distribution with 3 degrees of freedom 
}

# Part d)

ggplot(data = tibble(x = c(-2, 2)), aes(x = x)) + 
  stat_function(data = tibble(x = c(-2, 2)), fun = truncated_normal_density) +
  stat_function(data = tibble(x = c(-2, 2)), 
                fun = # FILL IN, color = "blue")

# Tutorial 10 Question 3 --------------------------------------------------

importance_sampling <- function(S, target, proposal, proposal_generator, func) {
  # Function to implement importance sampling
  # S: Number of desired samples from the target
  # target: A function that evaluates to a multiple of the target 
  # density (ch)
  # proposal: A function that evaluates to the proposal density (g)
  # proposal_generator: A function that gives a random sample from the proposal 
  # density
  
  sample_pts <- proposal_generator(S)
  weights <- # FILL IN
  estimate <- # FILL IN
  ess <- # FILL IN
  list(estimate = estimate, ess = ess)
}


# Tutorial 10 Question 4 --------------------------------------------------

log_mvt_beta_density <- function(x, alpha = 3, beta = 3) {
  sum(dbeta(x, shape1 = alpha, shape2 = beta, log = TRUE))
}

log_mvt_unif_density <- function(x) {
  sum(dunif(x, log = TRUE))
}

accept_rates <- rep(0, 10)
for (d in 1:10) {
  logM <- get_opt_logM(log_target = # FILL IN, 
                       log_proposal = # FILL IN,
                       init = rep(0.2, d))
  mvt_unif_proposal <- function() {
    runif(d)
  }
  accept_rates[[d]] <- # FILL IN
}

ggplot(data = tibble(x = 1:10, y = accept_rates)) + 
  geom_line(aes(x = x, y = y)) +
  scale_y_continuous(trans = 'log2')

# Tutorial 10 Question 5 --------------------------------------------------

P <- matrix(# FILL IN)
  


  