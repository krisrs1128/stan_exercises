## File Description -------------------------------------------------------------
## Example of putting prior on transformed parameters. From Section 20.2 of the
## Stan reference.

data {
  int<lower=1> N;
  real<lower=0, upper=1> theta[N];
}

parameters {
  real<lower=0> phi;
  real<lower=0> lambda;
}

model {
  phi ~ beta(1, 1);
  lambda ~ pareto(.1, 1.5);
  theta ~ beta(lambda * phi, lambda * (1 - phi));
}
