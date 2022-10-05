make_sales_figs <- function(n, seed=NULL) {
  set.seed(seed)
  base <- abs(rgamma(n, shape = 1, scale = 100))
  tibble(Dwight = base + sqrt(abs(rcauchy(n)) * 1000),
         Jim = base + sqrt(abs(rcauchy(n)) * 2500)
  ) %>% round()
}

get_tstat_perm_results <- function(x, y, nresample = 10000) {
  joint <- c(x, y)
  results <- rep(0, nresample)
  for (i in seq(nresample)) {
    joint_permuted <- sample(joint)
    x_new <- head(joint_permuted, length(x))
    y_new <- tail(joint_permuted, length(y))
    results[[i]] <- t.test(x_new, y_new, alternative = "greater")$statistic
  }
  results
}

get_tstat_perm_pval <- function(x, y, nresample = 10000) {
  tstat_perm_vals <- get_tstat_perm_results(x, y, nresample)
  mean(perm_results > t.test(x, y, alternative = "greater")$statistic)
}


# Empirical CDF -----------------------------------------------------------

make_ecdf_plot <- function(sampling_func, cdf_func, n_samples = c(10, 50, 200), 
                           left_lim = -4, right_lim = 4) {
  grid <- seq(left_lim, right_lim, length.out = 100)
  dfs = list()
  n_seeds <- 3
  for (j in seq(n_seeds)) {
    ecdf_fitted <- map(n_samples, ~ecdf(sampling_func(.)))
    for (i in seq_along(n_samples)) {
      dfs[[i + (j-1) * length(n_samples)]] <- tibble(x = grid, n = n_samples[[i]], rseed = j,
                         vals = ecdf_fitted[[i]](grid))
    }
  }
  plot_df <- bind_rows(dfs)
  cdf_plot_df <- tibble(x = grid, vals = cdf_func(grid))
  g <- ggplot(plot_df) + geom_step(aes(x = x, y = vals), color = "blue") + 
    geom_line(data = cdf_plot_df, mapping = aes(x = x, y = vals)) + 
    facet_grid(n ~ rseed)
  g
}

make_epd_plot <- function(sampling_func, pdf_func, n_samples = c(10, 50, 200), 
                          left_lim = -4, right_lim = 4) {
  grid <- seq(left_lim, right_lim, length.out = 100)
  dfs = list()
  n_seeds <- 3
  for (j in seq(n_seeds)) {
    samples <- map(n_samples, ~sampling_func(.))
    for (i in seq_along(n_samples)) {
      dfs[[i + (j-1) * length(n_samples)]] <- tibble(x = samples[[i]], n = n_samples[[i]], rseed = j, y = 0)
    }
  }
  plot_df <- bind_rows(dfs)
  pdf_plot_df <- tibble(x = grid, vals = pdf_func(grid))
  g <- ggplot(plot_df) + geom_point(aes(x = x, y = y), color = "blue", alpha = 0.25) + 
    geom_line(data = pdf_plot_df, mapping = aes(x = x, y = vals)) + 
    facet_grid(n ~ rseed)
  g
}
