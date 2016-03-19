
data {
  int<lower=2> K;
  int<lower=0> n;
  int<lower=1> p;
  int<lower=1, upper=K> y[n];
  vector[p] X[n];
}

transformed data {
  row_vector[p] zeros;
  zeros <- rep_row_vector(0, p);
}

parameters {
  matrix[K - 1, p] beta_raw;
}

model {
  matrix[K, p] beta;
  beta[1:(K - 1), ] <- beta_raw;
  for(j in 1:p) {
    beta[K, j] <- -sum(beta_raw[, j]);
  }

  to_vector(beta) ~ normal(0, 5); # diffuse prior
  for(i in 1:n) {
    y[i] ~ categorical_logit(beta * X[i]);
  }
}
