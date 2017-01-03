/* File description
 *
 * HMM estimation where the latent states are given. We model the sufficient
 * statistics, rather than the sequence of Categorical distributions. Based on
 * stan reference, section 9.6
 */

data {
  int<lower=1> K;
  int<lower=1> T;
  int<lower=1> V;
  int<lower=1, upper=K> z[T];
  int<lower=1, upper=V> w[T];
}

transformed data {
  int transition_count[K, K];
  int emission_count[K, V];

  ## Calculate sufficient statistics
  for (t in 1:(T - 1)) {
    transition_count[z[t], z[t + 1]] = 1 + transition_count[z[t], z[t + 1]];
  }
  for (t in 1:T) {
    emission_count[z[t], w[t]] = 1 + emission_count[z[t], w[t]];
  }
}

parameters {
  simplex[K] theta[K];
  simplex[V] phi[K];
}

model {
  for (k in 1:K) {
    transition_count[k] ~ multinomial(theta[k]);
    emission_count[k] ~ multinomial(phi[k]);
  }
}
