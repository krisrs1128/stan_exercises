#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Simple example estimating a gaussian process regression model

## ---- library-setup ----
library("ggplot2")
library("rstan")
library("plyr")
library("dplyr")
library("reshape2")

scale_colour_discrete <- function(...)
  scale_colour_brewer(..., palette = "Set2")
scale_fill_discrete <- function(...)
  scale_fill_brewer(..., palette = "Set2")

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

## ---- simulate-truth ----
## run the GPR simulation in gpr_sim.stan
N <- 250
D <- 1
x <- matrix(runif(N * D), N, D)

sim_data <- list(
  N = N,
  D = D,
  sigma = 0.3,
  eta = 10,
  rho = 10
)

stan_sim <- stan("gpr_sim.stan", data = sim_data, chains = 1)
sim_data <- extract(stan_sim)

## ---- estimate ----
stan_data <- list(
  N = N,
  D = D,
  x = x,
  y = sim_data$y[1000, ]
)
stan_fit <- stan("gpr_est.stan", data = stan_data, chains = 1)

## ---- posterior-fits ----
est_data <- extract(stan_fit)

posterior <- data.frame(
  iter = seq_len(nrow(est_data$eta_sq)),
  eta_sq = est_data$eta_sq,
  rho_sq = est_data$rho_sq,
  sigma_sq = est_data$sigma_sq
) %>%
  melt(id.vars = "iter", variable.name = "param")

ggplot(posterior %>% filter(iter > 500)) +
  geom_histogram(aes(x = value)) +
  xlim(0, 10) +
  facet_wrap(~param, scales = "free")
