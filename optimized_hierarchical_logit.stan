data {
  int<lower=1> D;
  int<lower=0> N;
  int<lower=1> L;
  int<lower=0, upper=1> y[N];
  int<lower=1, upper=L> ll[N];
  row_vector[D] x[N];
}

parameters {
  real mu[D];
  real<lower=0> sigma[D];
  vector[D] beta[L];
}

model {
  mu ~ normal(0, 100); # vague prior
  for(l in 1:L) {
    beta[l] ~ normal(mu, sigma);
  }

  # local variable speed up gives me a snytax error...
  # SYNTAX ERROR, MESSAGE(S) FROM PARSER:
  #
  # variable "vector" does not exist.
  #
  # ERROR at line 24
  for(n in 1:N) {
    y[n] ~ bernoulli_logit(x[n] * beta[ll[n]]);
  }

}
