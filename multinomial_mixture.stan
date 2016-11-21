data {
  int<lower=1> K; // num topics
  int<lower=1> V; // num words
  int<lower=0> D; // num docs
  int<lower=0> n[D, V]; // word counts for each doc

  // hyperparameters
  vector<lower=0>[K] alpha;
  vector<lower=0>[V] beta;
}

parameters {
  simplex[K] theta; // topic prevalence
  simplex[V] phi[K]; // word dist for k^th topic
}

transformed parameters {
  real gamma[D, K]; // log doc mixing parameters

  for (d in 1:D) {
    for (k in 1:K) {
      gamma[d, k] = categorical_lpmf(k | theta) + multinomial_lpmf(n[d] | phi[k]);
    }
  }
}

model {
  // priors
  theta ~ dirichlet(alpha);
  for (k in 1:K) {
    phi[k] ~ dirichlet(beta);
  }

  // likelihood
  for (d in 1:D) {
    target += log_sum_exp(gamma[d]);
  }
}
