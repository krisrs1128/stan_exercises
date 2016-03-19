data {
  int<lower=0> N;
  int<lower=0> K;
  real<lower=0> nu;
  matrix[N, K] X;
  vector[N] y;
}

parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
}

model {
  y ~ student_t(nu, X * beta + alpha, sigma);
}
