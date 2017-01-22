#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Experiment running gamma-poisson factorization model, on simulated data.
##
## reference:https://arxiv.org/pdf/1506.03431.pdf

## ---- libraries ----
library("plyr")
library("dplyr")
library("data.table")
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

## --- plot-utils ---
#' Plot Contours and Associated Coordinate
#'
#' This is a wrapper of ggcontours in the ggscaffold package that shows the
#' coordinate associated with point clouds.
#'
#' @param plot_data [data.frame] The data frame that contains the sampled
#'   coordinates for the contours, along with the true positions (which will be
#'   indicated with text).
#' @param plot_opts [list] A list specifying plotting appearance. Pretty much
#'   the same as the usual ggcontours plot_opts, except an extra option for plot
#'   limits.
scores_contours <- function(plot_data, plot_opts) {
  p1 <- ggcontours(plot_data, plot_opts) +
    geom_text(
      data = plot_data %>%
        group_by_(plot_opts$group) %>%
        summarise(mean_1 = mean(value_1), mean_2 = mean(value_2)),
      aes_string(x = "mean_1", y = "mean_2", label = plot_opts$group),
      col = plot_opts$mean_col,
      size = plot_opts$text_size
    ) +
    geom_text(
      data = plot_data %>% filter(iteration == 1),
      aes_string(x = "truth_1", y = "truth_2", label = plot_opts$group),
      size = plot_opts$text_size
    ) +
    scale_x_continuous(limits = plot_opts$x_lim, expand = c(0, 0)) +
    scale_y_continuous(limits = plot_opts$y_lim, expand = c(0, 0))
  p2 <- p1 +
    facet_wrap(formula(paste0("~", plot_opts$group)))
  list("grouped" = p1, "coordinates" = p2)
}

reshape_samples <- function(samples, truth, dims) {
  reshaped <- samples %>%
    melt(varnames = c("iteration", dims)) %>%
    left_join(
      melt(
        truth,
        varnames = dims,
        value.name = "truth"
      )
    )

  reshaped[, dims[1]] <- factor(
    reshaped[, dims[1]],
    order(truth[, 1])
  )

  reshaped %>%
    setDT() %>%
    dcast(
      sprintf("%s + iteration ~ %s", dims[1], dims[2]),
      value.var = c("value", "truth")
  )
}

## ---- examine ----
theta_fit <- reshape_samples(fit$theta, theta, c("i", "k"))

plot_opts <- list(
  "x" = "value_1",
  "y" = "value_2",
  "fill" = "log(..level..)",
  "fill_type" = "gradient",
  "group" = "i",
  "alpha" = 0.05,
  "h" = 0.3,
  "mean_col" = "#e34a33",
  "x_lim" = c(0, 6),
  "y_lim" = c(0, 8),
  "text_size" = 2,
  "panel_border" = 0.2
)

theta_plots <- scores_contours(theta_fit, plot_opts)
theta_plots$grouped

## ---- plot-beta ----
beta_fit <- reshape_samples(fit$beta, beta, c("v", "k"))

plot_opts$group <- "v"
beta_plots <- scores_contours(beta_fit, plot_opts)
beta_plots$grouped
