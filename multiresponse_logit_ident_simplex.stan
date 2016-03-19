
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
  simplex[K] beta_raw[p];
  vector<lower=0>[p] beta_scale;
}

transformed parameters {
  matrix[K, p] beta;
  for(k in 1:K) {
    for(j in 1:p) {
      beta[k, j] <- beta_scale[j] * (beta_raw[j][k] - 1.0 / K);
    }
  }
}

model {
  for(j in 1:p) {
    beta_raw[j] ~ dirichlet(rep_vector(1.0 / K, K));
  }
  beta_scale ~ normal(0, 5);

  for(i in 1:n) {
    y[i] ~ categorical_logit(beta * X[i]);
  }
}
