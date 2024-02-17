data {
  int<lower=1> N; // number of subjects
  int<lower=1> T; // number of trials
  int<lower=1> Ncs; // number of CSs
  real relief[N, T];
  int cs[N, T];

//   int anxious[N];

  real reward[Ncs];
  real tau; // relief scaling
}

parameters {
  // Hyper(group)-parameters
  real mu_alpha_pr;
  real<lower=0> sigma_alpha_pr;

  row_vector[Ncs] mu_initV_pr;
  row_vector<lower=0>[Ncs] sigma_initV_pr;

  real<lower=0> sigma;

//   real interaction;

  // Subject-level raw parameters (for Matt trick)
  vector[N] alpha_pr;    // learning rate
  matrix[N, Ncs] initV_pr;      // initV
}

transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[N] alpha;
  matrix<lower=0, upper=1>[N, Ncs] initV;

  for (i in 1:N) {
    alpha[i] = Phi_approx(mu_alpha_pr + /*anxious[i]*interaction +*/ sigma_alpha_pr * alpha_pr[i]);
    initV[i] = Phi_approx(mu_initV_pr + sigma_initV_pr .* initV_pr[i]);
  }
}

model {
  // Hyperparameters
  mu_alpha_pr ~ normal(0, 1);
  sigma_alpha_pr ~ normal(0, 0.2);
  mu_initV_pr ~ normal(0, 1);
  sigma_initV_pr ~ normal(0, 0.2);
  sigma ~ normal(0, 0.2);

//   interaction ~ normal(0, 1);

  // individual parameters
  alpha_pr ~ normal(0, 1);
  for(i in 1:N)
    initV_pr[i] ~ normal(0, 1);

  // subject loop and trial loop
  for (i in 1:N) {
    row_vector[Ncs] ev; // expected value
    real PE;      // prediction error

    ev = initV[i];

    for (t in 1:T) {
      // offset
      PE = (reward[cs[i, t]] - ev[cs[i, t]]);
      relief[i, t] ~ normal(tau * PE, sigma);
      ev[cs[i, t]] += alpha[i] * PE;
    }
  }
}

generated quantities {
  // For group level parameters
  real<lower=0, upper=1> mu_alpha;
  row_vector<lower=0, upper=1>[Ncs] mu_initV;

  // For posterior predictive check
  real y_pred[N, T];

  mu_alpha = Phi_approx(mu_alpha_pr);
  mu_initV = Phi_approx(mu_initV_pr);

  { // local section, this saves time and space
    for (i in 1:N) {
      row_vector[Ncs] ev; // expected value
      real PE;      // prediction error

      // Initialize values
      ev = initV[i];

      for (t in 1:T) {
        PE = (reward[cs[i, t]] - ev[cs[i, t]]);
        y_pred[i, t] = tau * PE;
        ev[cs[i, t]] += alpha[i] * PE;
      }
    }
  }
}
