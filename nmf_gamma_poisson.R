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
library("ggscaffold")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(01112017)

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

## ---- heatmap ----
y_df <- y %>%
  melt(
    varnames = c("row", "col"),
    value.name = "fill"
  )
plot_opts <- list(
  "x" = "row",
  "y" = "col",
  "x_order" = order(theta[, 1]),
  "y_order" = order(beta[, 1])
)

ggheatmap(y_df, plot_opts)

## ---- pca ----
compare_data <- data.frame(
  theta,
  princomp(scale(y))$scores
)

theme_set(min_theme())
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
fit <- extract(
  stan(file = "nmf_gamma_poisson.stan", data = stan_data, chains = 1)
)
save(fit, file = "nmf.rda")

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

theta_fit$i <- factor(
  theta_fit$i,
  levels = order(theta[, 1])
)

theta_fit_cast <- theta_fit %>%
  data.table::setDT() %>%
  data.table::dcast(i + iteration ~ k, value.var = c("value", "truth"))

plot_opts <- list(
  "x" = "value_1",
  "y" = "value_2",
  "fill" = "log(..level..)",
  "fill_type" = "gradient",
  "group" = "as.factor(i)",
  "alpha" = 0.1,
  "h" = 0.3
)

p <- ggcontours(theta_fit_cast, plot_opts) +
  geom_text(
    data = theta_fit_cast %>%
      group_by(i) %>%
      summarise(mean_1 = mean(value_1), mean_2 = mean(value_2)),
    aes(x = mean_1, y = mean_2, label = i),
    col = "#e34a33",
    size = 1.3
  ) +
  geom_text(
    data = theta_fit_cast %>% filter(iteration == 1),
    aes(x = truth_1, y = truth_2, label = i),
    size = 1.3
  ) +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 8))

## ---- faceted-thetas ----
p +
  facet_wrap(~i) +
  theme(
    panel.border = element_rect(fill = "transparent", size = .2)
  )

## ---- plot-beta ----
beta_fit <- melt(
  fit$beta,
  varnames = c("iteration", "v", "k")
) %>%
  left_join(
    melt(
      beta,
      varnames = c("v", "k"),
      value.name = "truth"
    )
  )

beta_fit$i <- factor(
  beta_fit$i,
  levels = order(beta[, 1])
)

beta_fit_cast <- beta_fit %>%
  data.table::setDT() %>%
  data.table::dcast(v + iteration ~ k, value.var = c("value", "truth"))

p <- ggplot() +
  stat_density2d(
    data = beta_fit_cast,
    aes(x = value_2, y = value_1, group = as.factor(v), fill = log(..level..)),
    geom = "polygon", alpha = 0.05, h = 0.4, bins = 40
  ) +
  geom_text(
    data = beta_fit_cast %>%
      group_by(v) %>%
      summarise(mean_1 = mean(value_2), mean_2 = mean(value_1)),
    aes(x = mean_1, y = mean_2, label = v),
    col = "#e34a33",
    size = 1.3
  ) +
  geom_text(
    data = beta_fit_cast %>% filter(iteration == 1),
    aes(x = truth_1, y = truth_2, label = v),
    size = 1.3
  ) +
  xlim(0, 6.5) +
  ylim(0, 7) +
  guides(fill = guide_legend(keywidth = 0.4, keyheight = 0.8, override.aes = list(alpha = 1))) +
  theme(
    axis.text = element_blank(),
    panel.spacing = unit(0, "line")
  ) +
  coord_fixed() +
  scale_fill_gradientn(colours = viridis(256, option = "D"), breaks = -3:0)
p

## ---- beta-facet ----
p +
  theme(
    axis.text = element_blank(),
    panel.spacing = unit(0, "line"),
    strip.text= element_blank(),
    panel.border = element_rect(fill = "transparent", size = .2)
  )
