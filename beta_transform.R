#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Example using the transformed parameters example
library("rstan")
library("plyr")
library("dplyr")
library("ggplot2")

stan(
  "beta_transform.stan",
  data = list(N = 1000, theta = rbeta(1000, 1, 1)),
  chains = 1
)
