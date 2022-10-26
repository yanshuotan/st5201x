
# Rejection sampling ------------------------------------------------------

rej_sampling_1d <- function(S, target, proposal, 
                            proposal_generator, M) {
  # S: Number of desired samples from the target
  # target: A function that evaluates to a multiple of the target 
  # density (ch)
  # proposal: A function that evaluates to the proposal density (g)
  # proposal_generator: A function that gives a random sample from the proposal 
  # density
  
  samples <- rep(0, S)
  s <- 0             # no. of samples collected
  T <- 0             # no. of iterations performed
  while (s < S){
    T <- T+1
    theta <- proposal_generator()
    U <- runif(1)
    if (M * proposal(theta) * U < target(theta)){
      s <- s+1
      samples[[s]] <- theta }}
  accept_rate <- S/T
  list(accept_rate = accept_rate, samples = samples)
}

rej_sampling_log_1d <- function(S, log_target, log_proposal, 
                            proposal_generator, logM) {
  # S: Number of desired samples from the target
  # log_target: A function that evaluates to log of a multiple of the target 
  # density (ch)
  # log_proposal: A function that evaluates to log of the proposal density (g)
  # proposal_generator: A function that gives a random sample from the proposal 
  # density
  
  samples <- rep(0, S)
  s <- 0             # no. of samples collected
  T <- 0             # no. of iterations performed
  while (s < S){
    T <- T+1
    theta <- proposal_generator()
    U <- runif(1)
    if (log(U) < log_proposal(theta) - log_target(theta) - logM){
      s <- s+1
      samples[[s]] <- theta }}
  accept_rate <- S/T
  list(accept_rate = accept_rate, samples = samples)
}

get_opt_M <- function(log_target, log_proposal, init) {
  diff <- function(x) {
    log_target(x) - log_proposal(x)
  }
  optim(init, fn = diff, control = list(fnscale=-1))
}

mystery_func <- function(x) {
  y <- dnorm(x, 0.25, 0.1) + dnorm(x, 0.75, 0.1) + 1
  y[x > 0 & x < 0] <- 0
  y
}

# Random walk  ------------------------------------------------------

in_circ <- function(X) {
  X[[1]] ** 2 + X[[2]] ** 2 < 1
}

rw_circle <- function(S, init = c(0, 0), stepsize = 0.2) {
  samples <- matrix(0, S, 2)
  s <- 1
  curr <- init
  samples[s, ] <- curr
  while (s < S){
    proposal <- rnorm(2, 0, stepsize)
    if (in_circ(curr + proposal)) {
      curr <- curr + proposal
      s <- s + 1
      samples[s, ] <- curr
    }
  }
  samples
}