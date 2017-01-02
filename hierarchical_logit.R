library("rstan")

matnorm <- function(n, p, sigma) {
  matrix(rnorm(n * p, 0, sigma), n, p)
}

# ordered logistic regression
beta0 <- rnorm(10)
B <- matrix(0, 10, 5)
y <- list()
X <- list()
for(l in seq_len(5)) {
  X[[l]] <- matnorm(100, 10, 1)
  B[, l] <- rnorm(10, beta0, .1)
  y[[l]] <- X[[l]] %*% B[, l] + rnorm(100, 0, .1) > 0
}

X <- do.call(rbind, X)
y <- as.numeric(unlist(y))
ll <- rep(1:5, each = 100)

stan("hierarchical_logit.stan",
     data = list(D = 10, N = 500, L = 5, y = y, ll = ll, x = X))
