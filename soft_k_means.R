#! /usr/bin/env Rscript

# File description -------------------------------------------------------------
# Generate data for soft-k-means example, and run rstan code for it
# https://github.com/stan-dev/stan/releases/download/v2.12.0/stan-reference-2.12.0.pdf
# page 153

## ---- setup ----
library("rstan")
library("data.table")
library("reshape2")
library("plyr")
library("dplyr")
library("ggplot2")

# Code Block -------------------------------------------------------------------

# generate data
N <- 100
K <- 2
p <- 3
mu <- matrix(rnorm(K * p, 0, 10), K, p)

z <- sample(1:K, N, replace = TRUE)
y <- matrix(0, N, p)
for (k in seq_len(K)) {
  y[z == k, ] <- matrix(1, sum(z == k), 1) %*% mu[k, , drop = F] +
    matrix(rnorm(sum(z == k) * p), sum(z == k), p)
}
heatmap(y)

# run STAN model
stan_data <- list(N = N, D = p, K = K, y = y)
m <- stan_model(file = "soft_k_means.stan")
n_iter <- 5000
stan_fit <- vb(m, data = stan_data, output_samples = n_iter)

# get samples
samples <- extract(stan_fit)
samples_mu <- melt(samples$mu)

# recovers means!
mu
ggplot(samples_mu %>% filter(iterations > 500)) +
  geom_histogram(aes(x = value, fill = as.factor(Var2))) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(Var3 ~ ., scale = "free_x")

samples_z <- melt(samples$soft_z, varnames = c("iterations", "n", "k"))
samples_z$truth <- z[samples_z$n]
head(samples_z)

samples_z_means <- samples_z %>%
  filter(iterations > 500) %>%
  group_by(n, k, truth) %>%
  summarise(mean = mean(value))

# recovers clusters!
ggplot(samples_z_means) +
  geom_histogram(aes(x = mean, fill = as.factor(k))) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(truth ~ .)
