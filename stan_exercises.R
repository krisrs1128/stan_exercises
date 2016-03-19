
################################################################################
# Exercises from the Stan user manual
################################################################################

library("rstan")
matnorm <- function(n, p, sigma) {
  matrix(rnorm(n * p, 0, sigma), n, p)
}

X <- matnorm(100, 10, 1)
beta <- rnorm(10)
y <- cbind(1, X) %*% c(2, beta) + rnorm(100, 0.5)
y <- as.numeric(y)

stan("linear_regression.stan",
     data = list(N = 100, K = 10, X = X, y = y))
beta
