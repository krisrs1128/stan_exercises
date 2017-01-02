## File description ------------------------------------------------------------
## Simulate data from Gaussian Process Regression
##
## Example from the Stan reference, section 17.2

data {
  int<lower=1> N; # number of samples
  int<lower=1> D; # dimension
  vector[D] x[N]; # covariates
  real<lower=0> sigma; # psd guarantee
  real<lower=0> eta; # importance of psd elem
  real<lower=0> rho; # bandwidth
}

transformed data {
  vector[N] mu;
  cov_matrix[N] Sigma;
  matrix[N, N] L;

  # create mean vector
  for (i in 1:N) {
    mu[i] = 0;
  }

  # Create covariance matrix
  for (i in 1:N) {
    for (j in 1:N) {
      Sigma[i, j] = eta ^ 2 * exp(-rho ^ 2 * dot_self(x[i] - x[j]));
      if (i == j) {
        Sigma[i, j] = Sigma[i, j] + sigma ^ 2;
      }
    }
  }
  L = cholesky_decompose(Sigma);
}

parameters {
  vector[N] z;
}

model {
  z ~ normal(0, 1);
}

generated quantities {
  vector[N] y;
  y = mu + L * z;
}
