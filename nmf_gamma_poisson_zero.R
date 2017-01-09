#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Experiment running gamma-poisson factorization model, on simulated data.
##
## reference:https://arxiv.org/pdf/1506.03431.pdf

## ---- libraries ----
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")
library("rstan")
options(mc.cores = parallel::detectCores())
set.seed(012017)

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

## set some proportion to zero
mask <- matrix(
  sample(c(0, 1), N * P, replace = T, prob = c(0.3, 0.7)),
  N, P
)
y[mask] <- 0

## ---- stan-fit ----
f <- stan_model("nmf_gamma_poisson.stan")
fit <- extract(
  vb(f, data = stan_data)
)

## ---- examine ----
theta_fit <- melt(
  fit$theta,
  varnames = c("iteration", "i", "k")
) %>%
  left_join(
    melt(
      theta,
      varnames = c("i", "k"),
      value.name = "truth"
    )
  )

theta_levels <- theta_fit %>%
  group_by(i, k) %>%
  summarise(mean = mean(value)) %>%
  filter(k == 1) %>%
  arrange(desc(mean)) %>%
  select(i) %>%
  unlist()

theta_fit$i <- factor(
  theta_fit$i,
  levels = theta_levels
)

ggplot(
  theta_fit %>%
  filter(as.numeric(i) <= 25)
) +
  geom_histogram(
    aes(x = value, fill = as.factor(k)),
    bins = 100, alpha = 0.6) +
  coord_flip() +
  facet_grid(. ~ i) +
  xlim(0, 6) +
  theme(
    panel.spacing = unit(0, "line")
  ) +
  geom_vline(
    aes(xintercept = truth, col = as.factor(k))
  )
