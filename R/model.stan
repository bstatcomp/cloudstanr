data {
  int<lower=1> n; // number of values
  real y[n]; // values
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma);
}
