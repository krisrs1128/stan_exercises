
data {
  int<lower=1> J; // students
  int<lower=1> K; // questions
  int<lower=1> N; // observations
  int<lower=1, upper=J> jj[N];
  int<lower=1, upper=K> kk[N];
  int<lower=0, upper=1> y[N];
}

parameters {
  real delta;// mean ability
  real alpha[J]; // deviation from mean, per student
  real beta[K]; // difficulty of questions
}


model {
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  delta ~ normal(.75, 1);

  for (n in 1:N) {
    y[n] ~ bernoulli_logit(delta + alpha[jj[n]] - beta[kk[n]]);
  }  
}
