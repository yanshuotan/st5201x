bootstrap0 <- function(x, B = 1000, func, ...) {
  # x is data vector or matrix (with each row a case)
  # B is number of bootstrap replications
  # func is R function that inputs a data vector or
  # matrix and returns a numeric number or vector
  # ... other arguments for func
  x <- as.matrix(x)
  n <- nrow(x)
  f0 <- func(x,...) # get size of output
  T_boot <- matrix(0, length(f0), B) # create matrix to store T_boot results
  for (b in 1:B) {
    i <- sample(1:n, n, replace = TRUE)
    T_boot[, b] <- func(x[i, ],...)
  }
  drop(T_boot) # drop unnecessary dimensions, i.e. if length(f0) = 1
}

double_bootstrap0 <- function(x, B = 1000, func, B_double = 100, ...) {
  # x is data vector or matrix (with each row a case)
  # B is number of bootstrap replications
  # func is R function that inputs a data vector or
  # matrix and returns a numeric number or vector
  # ... other arguments for func
  x <- as.matrix(x)
  n <- nrow(x)
  T_boot <- vector("numeric", B)
  T_boot_var <- vector("numeric", B)
  for (b in 1:B) {
    i <- sample(1:n, n, replace = TRUE)
    x_boot <- as.matrix(x[i, ])
    T_boot[[b]] <- func(x_boot, ...)
    T_double_boot <- vector("numeric", B_double)
    for (a in 1:B_double) {
      j <- sample(1:n, n, replace = TRUE)
      x_double_boot <- as.matrix(x_boot[j, ])
      T_double_boot[[a]] <- func(x_double_boot, ...)
    }
    T_boot_var[[b]] <- var(T_double_boot)
  }
  tibble(T_boot = T_boot, T_boot_var = T_boot_var)
}

# Confidence intervals ----------------------------------------------------

normal_interval <- function(x, B, func, alpha, ...) {
  T <- func(x)
  T_boot <- bootstrap0(x, B = B, func)
  se_boot <- sqrt(var(T_boot))
  lower <- T - qnorm(1 - alpha/2) * se_boot
  upper <- T + qnorm(1 - alpha/2) * se_boot
  ci <- c(lower, upper)
  ci
}

percentile_interval <- function(x, B, func, alpha, ...) {
  T <- func(x)
  T_boot <- bootstrap0(x, B = B, func)
  lower <- quantile(T_boot, alpha/2)
  upper <- quantile(T_boot, 1 - alpha/2)
  ci <- c(lower, upper)
  names(ci) <- NULL
  ci
}

pivotal_interval <- function(x, B, func, alpha, ...) {
  T <- func(x)
  T_boot <- bootstrap0(x, B = B, func)
  lower <- 2 * T - quantile(T_boot, 1 - alpha/2)
  upper <- 2 * T - quantile(T_boot, alpha/2)
  ci <- c(lower, upper)
  names(ci) <- NULL
  ci
}

studentized_pivotal_interval <- function(x, B, func, alpha, 
                                         B_double = 100, ...) {
  T <- func(x)
  boot_data_df <- double_bootstrap0(x, B, func, B_double)
  Z_boot <- (boot_data_df$T_boot - T) / sqrt(boot_data_df$T_boot_var)
  se_boot <- sqrt(var(boot_data_df$T_boot))
  lower <- T - quantile(Z_boot, 1 - alpha/2) * se_boot
  upper <- T - quantile(Z_boot, alpha/2) * se_boot
  ci <- c(lower, upper)
  names(ci) <- NULL
  ci
}