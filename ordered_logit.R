
library("rstan")

matnorm <- function(n, p, sigma) {
  matrix(rnorm(n * p, 0, sigma), n, p)
}

# ordered logistic regression
X <- matnorm(500, 10, 1)
beta <- rnorm(10)

eta <- cbind(1, X) %*% c(2, beta) + rnorm(500, 0.5)
c_param <- c(-1, 1)

y <- rep(2, length(eta))
y[eta < c_param[1]] <- 1
y[eta > c_param[2]] <- 3

stan("ordered_logit.stan",
     data = list(K = 3, N = 500, D = 10, x = X, y = y))
