
data {
  int<lower=1> J; // students
  int<lower=1> K; // questions
  int<lower=1> N; // observations
  int<lower=1, upper=J> jj[N];
  int<lower=1, upper=K> kk[N];
  int<lower=0, upper=1> y[N];
}

parameters {
  real mu_beta; // mean ability
  real alpha[J]; // deviation from mean, per student
  real beta[K]; // difficulty of questions
  real<lower=0> gamma[K]; // discrimination for current question
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
}


model {
  alpha ~ normal(0, 1);
  beta ~ normal(0, sigma_beta);
  gamma ~ lognormal(0, sigma_gamma);
  mu_beta ~ cauchy(0, 5);
  sigma_beta ~ cauchy(0, 5);
  sigma_gamma ~ cauchy(0, 5);

  for (n in 1:N) {
    y[n] ~ bernoulli_logit(gamma[kk[n]] * (alpha[jj[n]] - (beta[kk[n]] + mu_beta)));
  }  
}
