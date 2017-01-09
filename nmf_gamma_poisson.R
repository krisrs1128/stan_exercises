#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Experiment running gamma-pmesoisson factorization model, on simulated data.
##
## reference:https://arxiv.org/pdf/1506.03431.pdf

## ---- libraries ----
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")
library("rstan")
options(mc.cores = parallel::detectCores())

## ---- simulate ----
stan_data <- list(
  K = 2,
  N = 100,
  P = 10,
  a = 1,
  b = 1,
  c = 1,
  d = 1
)
attach(stan_data)

## scores
theta <- matrix(
  rgamma(N * K, rate = a, shape = b),
  N, K
)

## factors
beta <- matrix(
  rgamma(P * K, rate = a, shape = b),
  P, K
)

## observations
y <- matrix(
  rpois(N * P, theta %*% t(beta)),
  N, P
)

## ---- stan-fit ----
f <- stan_model("nmf_gamma_poisson.stan")
fit <- extract(
  vb(f, data = stan_data)
)

## ---- examine ----
theta_fit <- melt(
  fit$theta,
  varnames = c("iteration", "i", "k")
)

ggplot(theta_fit) +
  geom_histogram(aes(x = iterations)) +
  facet_wrap(k ~ i)
