# Finite state space trajectories -----------------------------------------

make_rw_transition_matrix <- function(n_vertices, edges, lazy = FALSE) {
  # Function to make a transition matrix for a RW on a graph
  # n_vertices: number of vertices the graph
  # edges: A list of tuples representing the edges in the graph
  # lazy: If TRUE, return transition matrix for the lazy RW (move only with
  # probability 1/2)
  P <- matrix(0, nrow = n_vertices, ncol = n_vertices)
  for (e in edges) {
    P[e[[1]], e[[2]]] <- 1
    P[e[[2]], e[[1]]] <- 1
  }
  P <- P / apply(P, 1, sum)
  if (lazy) {
    P <- (P + diag(rep(1, n_vertices))) / 2
  }
  P
}

sample_trajectory <- function(T, P, x0) {
  # Function to sample a trajectory from a MC
  # T: The length of the trajectory to sample
  # P: The transition matrix
  # x0: The initial state
  n_states <- ncol(P)
  trajectory <- rep(0, T + 1)
  xt <- x0
  trajectory[[1]] <- xt
  for (t in 1:T) {
    xt <- sample(1:n_states, 1, prob = P[xt, ])
    trajectory[[t+1]] <- xt
  }
  trajectory
}

plot_trajectory <- function(trajectory) {
  # Function to plot a MC trajectory
  ggplot(data = tibble(t = 0:(length(trajectory) - 1),
                       y = trajectory)) + 
    geom_line(aes(x = t, y = y), color = "blue") + ylab("state")
}

# Metropolis-Hastings on finite state space -------------------------------

sample_mh_trajectory <- function(T, P, x0, pi) {
  # Function to sample a trajectory from a MC
  # T: The length of the trajectory to sample
  # P: The transition matrix
  # x0: The initial state
  n_states <- ncol(P)
  trajectory <- rep(0, T + 1)
  xt <- x0
  trajectory[[1]] <- xt
  for (t in 1:T) {
    x_prop <- sample(1:n_states, 1, prob = P[xt, ])
    alpha <- min(pi[[x_prop]] * P[[x_prop, xt]] / 
                   (pi[[xt]] * P[[xt, x_prop]]), 1)
    U <- runif(1)
    if (U < alpha) {
      xt <- x_prop
    }
    trajectory[[t+1]] <- xt
  }
  trajectory
}

make_mh_transition_matrix <- function(P, pi) {
  # Function to create the updated transition matrix when we apply the MH
  # filter to an existing transition matrix P with target distribution pi
  n_states <- length(pi)
  Q <- matrix(0, nrow = n_states, ncol = n_states)
  for (i in 1:n_states) {
    for (j in 1:n_states) {
      if (P[[i, j]] > 0) {
        ratio <- pi[[j]] * P[[j, i]] / (pi[[i]] * P[[i, j]])
        Q[[i, j]] <- P[[i, j]] * min(ratio, 1)
      }
    }
    Q[[i, i]] <- 1 - sum(Q[i, ])
  }
  Q
}


# Metropolis-Hastings on continuous state space ---------------------------

rwmetrop0 <- function(target, stepsize, x0, T, log = FALSE) {
  # Function to compute the MH trajectory on continuous state space
  # target: The target density to sample from
  # stepsize: The step size (std) for the proposal normal distribution
  # x0: The initial state
  # T: The total number of time steps / iterations
  # log: TRUE if target is supplied in log density form, FALSE otherwise
  
  d <- length(x0)
  trajectory <- matrix(0, nrow = (T + 1), ncol = d) # For storing results
  accept_count <- 0 
  # Initialize trajectory
  xt <- x0
  trajectory[1, ] <- xt
  for (t in 1:T) {
    x_prop <- xt + rnorm(d, sd = stepsize)
    # MH filter
    if (log) {
      r <- exp(target(x_prop) - target(xt))
    } else {
      r <- target(x_prop) / target(xt)
    }
    alpha <- min(r, 1)
    U <- runif(1)
    if (U < alpha) {
      xt <- x_prop
      accept_count <- accept_count + 1
    }
    trajectory[t+1, ] <- xt
  }
  list(par = drop(trajectory), accept = accept_count / T)
}