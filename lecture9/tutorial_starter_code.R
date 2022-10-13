set.seed(42)
nsamples <- 5
nbins <- 10
X <- rnorm(50)
n_iter <- 1000
fitted_densities <- vector("list", n_iter)
for (i in seq(n_iter)) {
  X_boot <- sample(X, replace = TRUE)
  fitted_densities[[i]] <- density(X_boot)
}
density_vals <- matrix(0, n_iter, length(fitted_densities[[1]]$y))
for (i in seq(n_iter)) {
  density_vals[i, ] = fitted_densities[[i]]$y
}
confidence_bands <- apply(density_vals, 2, quantile, c(0.025, 0.975))
plot_df <- tibble(x = fitted_densities[[1]]$x, lower = confidence_bands[1, ], upper = confidence_bands[2, ])

ggplot(plot_df) + geom_line(aes(x = x, y = lower), color = "red", linetype = 2) + 
  geom_line(aes(x = x, y = upper), color = "red", linetype = 2) + 
  geom_density(data = tibble(x = X), mapping = aes(x = x))