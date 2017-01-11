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
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(01112017)

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
  P = 75,
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

## ---- pca ----
compare_data <- data.frame(
  theta,
  princomp(scale(y))$scores
)

ggplot(compare_data) +
  geom_point(aes(x = Comp.1, Comp.2, size = X1, col = X2))

## ---- overdispersion ----
yy <- sort(rpois(N * P, mean(y)))
qq_df <- data.frame(
  y = c(sort(y), yy),
  label = c(rep("gamma-poisson", N * P), rep("poisson", N * P))
)

ggplot(qq_df) +
  geom_histogram(aes(x = y), binwidth = .5) +
  facet_grid(label ~ .)

ggplot(data.frame(
  mu = rowMeans(y),
  sigma = apply(y, 1, sd)
)) +
  geom_point(
    aes(x = mu, y = sigma)
  ) +
  coord_fixed() +
  geom_abline(slope = 1)

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

theta_fit_cast <- theta_fit %>%
  data.table::setDT() %>%
  data.table::dcast(i + iteration ~ k, value.var = c("value", "truth"))

ggplot() +
  geom_text(
    data = theta_fit_cast %>% filter(iteration > 100),
    aes(x = value_1, y = value_2, label = i),
    size = 2, alpha = 0.1, col = "#5E5E5E"
  ) +
  geom_text(
    data = theta_fit_cast %>% filter(iteration == 1),
    aes(x = truth_1, y = truth_2, label = i),
    size = 5, alpha = 1, col = "#d95f02"
  ) +
  theme(
    axis.text = element_blank(),
    panel.spacing = unit(0, "line")
  )

ggplot() +
  geom_text(
    data = theta_fit_cast %>% filter(iteration > 100),
    aes(x = value_1, y = value_2, label = i),
    size = 2, alpha = 0.1, col = "#5E5E5E"
  ) +
  geom_text(
    data = theta_fit_cast %>% filter(iteration == 1),
    aes(x = truth_1, y = truth_2, label = i),
    size = 5, alpha = 1, col = "#d95f02"
  ) +
  facet_wrap(~i) +
  theme(
    axis.text = element_blank(),
    panel.spacing = unit(0, "line"),
    strip.text= element_blank(),
    panel.border = element_rect(fill = "transparent", size = .2)
  )
