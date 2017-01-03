## File Description -----
/* File Description
 *
 * HMM estimation where the latent states are given. We model the sufficient
 * statistics, rather than the sequence of Categorical distributions. Based on
 * stan reference, section 9.6
 */

data {
  int<lower=1> K;
  int<lower=1> T;
  int<lower=1> N;
  int<lower=1, upper=K> z[T];
  int<lower=1, upper=N> w[T];
}

parameters {
  
}

model {
  
}
