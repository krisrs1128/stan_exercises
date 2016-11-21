#! /usr/bin/env Rscript

# File description -------------------------------------------------------------
# Generate data for multinomial mixture model, and run rstan code for it
# Based on
# https://github.com/stan-dev/stan/releases/download/v2.12.0/stan-reference-2.12.0.pdf
# page 157

## ---- setup ----
library("rstan")
library("data.table")
library("reshape2")
library("plyr")
library("dplyr")
library("ggplot2")

# Code Block -------------------------------------------------------------------

# generate data
D <- 50
K <- 2
V <- 5
phi <- matrix(rgamma(K * V, 1), K, V)
words_per_doc <- 50

for (k in seq_len(K)) {
  phi[k, ] <- phi[k, ] / sum(phi[k, ])
}

z <- sample(1:K, D, replace = TRUE)
n <- matrix(0, D, V)

for (k in seq_len(K)) {
  nk <- sum(z == k)
  n[z == k, ] <- t(rmultinom(nk, words_per_doc, phi[k, ]))
}

image(t(n[order(z), ]))

# run STAN model
stan_data <- list(
  K = K,
  V = V,
  D = D,
  n = n,
  alpha = rep(.01, K),
  beta = rep(.01, V)
)

m <- stan_model(file = "multinomial_mixture.stan")
n_iter <- 1000
stan_fit <- stan(file = "multinomial_mixture.stan", data = stan_data, iter = n_iter)

# get samples
samples <- extract(stan_fit)

# Recovers phi
samples_phi <- melt(samples$phi)
phi_hat <- samples_phi %>%
  group_by(Var2, Var3) %>%
  summarise(mean = mean(value)) %>%
  dcast(Var2 ~ Var3)

phi
phi_hat

# recovered cluster memberships
samples_gamma <- melt(samples$gamma, varnames= c("iteration", "n", "k"))
samples_gamma$value[!is.finite(samples_gamma$value)] <- NA
gamma_hat <- samples_gamma %>%
  group_by(n, k) %>%
  summarise(mean = mean(value, na.rm = TRUE))

gamma_hat$truth <- z[gamma_hat$n]
head(gamma_hat)

ggplot(gamma_hat) +
  geom_histogram(aes(x = mean, fill = as.factor(k))) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(truth ~ .)
