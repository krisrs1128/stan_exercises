
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

stan("robust_linear_regression.stan",
     data = list(N = 100, K = 10, nu = 1, X = X, y = y))

# logistic regression
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

n <- 200
x <- rnorm(n)
alpha <- .2
beta <- 1
p <- inv_logit(alpha + beta * x)
y <- vector(length = n)
for(i in seq_len(n)) {
  y[i] <- sample(0:1, size = 1, prob = c(1 - p[i], p[i]))
}

stan("logit_one_dim.stan",
     data = list(N = n, x = x, y = y))

# multiple outcome logistic regression
softmax <- function(x) {
  exp(x) / sum(exp(x))
}

N <- 400
K <- 3
p <- 5
beta <- matnorm(K, p, 1)
beta[K, ] <- 0
X <- matnorm(n, p, 1)
P <- t(apply(X %*% t(beta), 1, softmax))
for(i in seq_len(n)) {
  y[i] <- sample(1:K, 1, prob = P[i, ])
}

stan("multiresponse_logit.stan",
     data = list(n = n, K = K, X = X, y = y),
     chains = 2)
beta

# restrict last level to be 0
stan("multiresponse_logit_ident.stan",
     data = list(n = n, K = K, X = X, y = y),
     chains = 2)
beta

# restrict sum to be zero
stan("multiresponse_logit_ident_sum.stan",
     data = list(n = n, K = K, X = X, y = y),
     chains = 2)
beta

# use scaled simplex
stan("multiresponse_logit_ident_simplex.stan",
     data = list(n = n, K = K, X = X, y = y),
     chains = 2)
beta
