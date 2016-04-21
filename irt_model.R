#! /usr/bin/env Rscript

# File description -------------------------------------------------------------

# Setup packages ---------------------------------------------------------------
# List of packages for session
.packages  <-  c("rstan",
                 "reshape2")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(any(!.inst)) {
  install.packages(.packages[!.inst], repos = "http://cran.rstudio.com/")
}

# Load packages into session 
lapply(.packages, require, character.only=TRUE)
cat("\014")  # Clear console

# General setup ----------------------------------------------------------------
rm(list=ls()) # Delete all existing variables
graphics.off() # Close all open plots

# Code Block -------------------------------------------------------------------
n <- 100 # students
k <- 20 # questions
alpha <- rnorm(n)
beta <- rnorm(k)
delta <- rnorm(1, .75, 1)
z <- delta * matrix(1, n, k) + alpha %*% matrix(1, 1, k) +
  matrix(1, n, 1) %*% t(beta)

inv_logit <- function(z) {
  exp(z) / (1 + exp(z))
}

p <- inv_logit(z)
Y <- matrix(0, n, k)
for (i in 1:nrow(Y)){
  for (j in 1:ncol(Y)) {
    Y[i, j] <- sample(0:1, size = 1, prob = c(1 - p[i, j], p[i, j]))
  }
}

m_y <- melt(Y)

summary(m_y)

jj <- m_y$Var1
kk <- m_y$Var2
y <- m_y$value

stan_data <- list(y = y, jj = jj, kk = kk, J = n, K = k, N = n * k)
stan_fit <- stan(file = "irt_model.stan", data = stan_data)
stan_fit

# Tidy things up ---------------------------------------------------------------
cat("\014")  # Clear console

# Scratchpad -------------------------------------------------------------------
# print( "Hello, world!")
