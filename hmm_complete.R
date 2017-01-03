#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Simulate data to experiment with Stan implementation of HMM.

## ---- libraries ----
library("rstan")
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")

## ---- themes ----
scale_colour_discrete <- function(...)
  scale_colour_brewer(..., palette="Set2")
scale_fill_discrete <- function(...)
  scale_fill_brewer(..., palette="Set2")

theme_set(theme_bw())
min_theme <- theme_update(
  panel.border = element_blank(),
  panel.grid = element_blank(),
  text = element_text(family = "Ubuntu Regular", color = "#22211d"),
  axis.ticks = element_blank(),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 6),
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 8),
  strip.background = element_blank(),
  strip.text = element_text(size = 8),
  legend.key = element_blank()
)

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
fit <- extract(
  stan("hmm_complete.stan", data = stan_data)
)

## ---- analyze results ----
theta_fit <- melt(
  as.array(fit$theta),
  varnames = c("iteration", "start", "end")
)

phi_fit <- melt(
  as.array(fit$phi),
  varnames = c("iteration", "k", "n")
)

ggplot(theta_fit) +
  geom_bar(aes(x = end, y = value), stat = "identity") +
  facet_grid(start ~ .)

ggplot(phi_fit) +
  geom_bar(aes(x = v, y = value), stat = "identity") +
  facet_grid(k ~ .)
