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
D <- 1000
K <- 3
V <- 10
phi <- matrix(rgamma(K * V, 10), K, V)
words_per_doc <- 100

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
  z = z,
  n = n,
  alpha = rep(1, K),
  beta = rep(1, V)
)

m <- stan_model(file = "multinomial_mixture.stan")
n_iter <- 1000
stan_fit <- vb(m, data = stan_data, output_samples = n_iter)

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
