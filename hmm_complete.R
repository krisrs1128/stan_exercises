#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Simulate data to experiment with Stan implementation of HMM.

## ---- libraries ----
library("rstan")
library("plyr")
library("dplyr")

## ---- simulate-data ----
K <- 4
T <- 25
V <- 10
z <- vector(length = T)
w <- vector(length = T)

## parameters
theta <- matrix(rgamma(K * K, shape = 1), K, K)
theta <- diag(1 / rowSums(theta)) %*% theta

phi <- matrix(rgamma(V * K, shape = 1), K, V)
phi <- diag(1 / rowSums(phi)) %*% phi

## simulate states
z[1] <- sample(seq_len(K), 1)
for (i in seq_len(T - 1)) {
  z[i + 1] <- sample(seq_len(K), size = 1, prob = theta[z[i], ])
}

## simulate words
for (i in seq_len(T)) {
  w[i]  <- sample(seq_len(V), size = 1, prob = phi[z[i], ]) 
}

## ---- estimate-parameters ----
stan_data <- list(
  K = K,
  T = T,
  V = V,
  z = z,
  w = w,
  alpha = rep(1, K),
  beta = rep(1, V)
)
stan("hmm_complete.stan", data = stan_data)
