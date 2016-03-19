
data {
  int<lower=2> K;
  int<lower=0> n;
  int<lower=1> p;
  int<lower=1, upper=K> y[n];
  vector[p] X[n];
}

parameters {
  matrix[K, p] beta;
}

model {
  to_vector(beta) ~ normal(0, 5); # diffuse prior
  for(i in 1:n) {
    y[i] ~ categorical_logit(beta * X[i]);
  }
}
