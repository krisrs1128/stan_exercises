## File description --------------------------------

data {
  int<lower=1> N;
  int<lower=1> D;
  vector[D] x[N];
  real y[N];
}

transformed data {
  vector[N] mu;
  for (i in 1:N) {
    mu[i] = 0;
  }
}

parameters {
  real<lower=0> eta_sq;
  real<lower=0> inv_rho_sq;
  real<lower=0> sigma_sq;
}

transformed parameters {
  real<lower=0> rho_sq;
  rho_sq = inv(inv_rho_sq);
}

model {
  matrix[N, N] Sigma;

  # off diagonal elements
  for (i in 1:(N - 1)) {
    for (j in 1:i) {
      Sigma[i, j] = eta_sq * exp(-rho_sq * dot_self(x[i] - x[j]));
      Sigma[j, i] = Sigma[i, j];
    }
  }

  # diagonal elements
  for (i in 1:N) {
    Sigma[i, i] = eta_sq + sigma_sq;
  }

  # hyperpriors
  inv_rho_sq ~ cauchy(0, 5);
  eta_sq ~ cauchy(0, 5);
  sigma_sq ~ cauchy(0, 5);
}
