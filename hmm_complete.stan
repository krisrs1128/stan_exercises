/* File description
 *
 * HMM estimation where the latent states are given. We model the sufficient
 * statistics, rather than the sequence of Categorical distributions. Based on
 * stan reference, section 9.6
 */

functions {
  /* Calculate emissions sufficient statistics
   *
   * @param z The array of underlying states.
   * @param w The array of words observed at each time.
   * @param K The number of latent states.
   * @param V The size of the vocabulary.
   * @param T The length of the sequence.
   * @return count The K x V matrix of counts of words associated with each
   *   state.
   */
  int[,] emissions(int[] z, int[] w, int K, int V, int T) {
    int count[K, V];
    for (k in 1:K) {
      for (v in 1:V) {
        count[k, v] = 0;
      }
    }

    for (t in 1:T) {
      count[z[t], w[t]] = 1 + count[z[t], w[t]];
    }
    return count;
  }
}

data {
  int<lower=1> K;
  int<lower=1> T;
  int<lower=1> V;
  int<lower=1, upper=K> z[T];
  int<lower=1, upper=V> w[T];
  vector<lower=0>[K] alpha;
  vector<lower=0>[V] beta;
}


transformed data {

  ## Intitialize structures for sufficient statistics
  int<lower=0> transition_count[K, K];
  int<lower=0> emission_count[K, V];
  for (k in 1:K) {
    for (k_tilde in 1:K) {
      transition_count[k, k_tilde] = 0;
    }
  }

  ## Calculate sufficient statistics
  for (t in 1:(T - 1)) {
    transition_count[z[t], z[t + 1]] = 1 + transition_count[z[t], z[t + 1]];
  }
  emission_count = emissions(z, w, K, V, T);
}

parameters {
  simplex[K] theta[K];
  simplex[V] phi[K];
}

model {
  for (k in 1:K) {
    // prior probabilities
    theta[k] ~ dirichlet(alpha);
    phi[k] ~ dirichlet(beta);

    // exponential families
    transition_count[k] ~ multinomial(theta[k]);
    emission_count[k] ~ multinomial(phi[k]);
  }
}
