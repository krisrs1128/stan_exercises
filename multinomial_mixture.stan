data {
  int<lower=1> K; // num topics
  int<lower=1> V; // num words
  int<lower=0> D; // num docs
  int<lower=1, upper=K> z[D]; // topic for each doc

  int<lower=0> n[D, V]; // word counts for each doc

  // hyperparameters
  vector<lower=0>[K] alpha;
  vector<lower=0>[V] beta;
}

parameters {
  simplex[K] theta; // topic prevalence
  simplex[V] phi[K]; // word dist for k^th topic
}

model {
  theta ~ dirichlet(alpha);

  for (k in 1:K) {
    phi[k] ~ dirichlet(beta);
  }

  for (d in 1:D) {
    z[d] ~ categorical(theta);
  }

  for (d in 1:D) {
    n[d] ~ multinomial(phi[z[d]]);
  }
}
