#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## setup libraries / themes
library("rstan")
library("ggplot2")
library("plyr")
library("dplyr")
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

## run the GPR simulation in gpr_sim.stan
N <- 100
D <- 1
x <- matrix(runif(N * D), N, D)

stan_data <- list(
  N = N,
  D = D,
  sigma = 0.3,
  eta = 1,
  rho = 1 
)

stan_fit <- stan("gpr_sim.stan", data = stan_data, chains = 1)

## study results
stan_list <- extract(stan_fit)
plot_data <- stan_list$y %>%
  melt(varnames = c("iteration", "n"), value.name = "y") %>%
  left_join(data.frame(n = 1:N, x = x))

ggplot(plot_data %>% filter(iteration < 10)) +
  geom_point(aes(x = x, y = y)) +
  facet_wrap(~iteration)

