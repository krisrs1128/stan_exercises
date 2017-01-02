## File Description -------------------------------------------------------------
## Example of putting prior on transformed parameters. From Section 20.2 of the
## Stan reference.

parameters {
  real<lower=0> phi;
  real<lower=0> lambda;
}

transformed parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  phi ~ beta(1, 1);
  lambda ~ pareto(.1, 1.5);
  theta ~ beta(lambda * phi, lambda * (1 - phi));
}
