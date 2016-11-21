#! /usr/bin/env Rscript

# File description -------------------------------------------------------------
# Generate data for soft-k-means example, and run rstan code for it
# https://github.com/stan-dev/stan/releases/download/v2.12.0/stan-reference-2.12.0.pdf
# page 153

## ---- setup ----
library("rstan")

# Code Block -------------------------------------------------------------------

# generate data
N <- 500
K <- 4
p <- 5
mu <- matrix(rnorm(K * p, 10), K, p)

z <- sample(1:K, N, replace = TRUE)
X <- matrix(0, N, p)
for (k in seq_len(K)) {
  X[z == k, ] <- matrix(1, sum(z == k), 1) %*% mu[k, , drop = F] +
    matrix(rnorm(sum(z == k) * p), sum(z == k), p)
}
heatmap(X)

# run STAN model
