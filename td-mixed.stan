data {
  int<lower=1> N; // number of subjects
  int<lower=1> T; // number of trials
  int<lower=1> O; // number of offset
  int<lower=1> Ncs; // number of CSs
  real relief[N, T, O];
  int cs[N, T];

  // int anxious[N];

  real reward[Ncs];
  real tau; // relief scaling
}

parameters {
  // Hyper(group)-parameters
  real mu_alpha_pr;
  real<lower=0> sigma_alpha_pr;

  real mu_gamma_pr;
  real<lower=0> sigma_gamma_pr;

  row_vector[Ncs] mu_initV_pr;
  row_vector<lower=0>[Ncs] sigma_initV_pr;

  real<lower=0> sigma;

//   real interaction;

  // Subject-level raw parameters (for Matt trick)
  vector[N] alpha_pr;    // learning rate
  vector[N] gamma_pr;    // reward sensitivity
  matrix[N, Ncs] initV_pr;      // initV
}

transformed parameters {
  // subject-level parameters
  vector<lower=0, upper=1>[N] alpha;
  vector<lower=0, upper=1>[N] gamma;
  matrix<lower=0, upper=1>[N, Ncs] initV;

  for (i in 1:N) {
    alpha[i] = Phi_approx(mu_alpha_pr + /*anxious[i]*interaction +*/ sigma_alpha_pr * alpha_pr[i]);
    gamma[i] = Phi_approx(mu_gamma_pr + /*anxious[i]*interaction +*/ sigma_gamma_pr * gamma_pr[i]);
    initV[i] = Phi_approx(mu_initV_pr + sigma_initV_pr .* initV_pr[i]);
  }
}

model {
  // Hyperparameters
  mu_alpha_pr ~ normal(0, 1);
  sigma_alpha_pr ~ normal(0, 0.2);
  mu_gamma_pr ~ normal(0, 1);
  sigma_gamma_pr ~ normal(0, 0.2);
  mu_initV_pr ~ normal(0, 1);
  sigma_initV_pr ~ normal(0, 0.2);
  sigma ~ normal(0, 0.2);

  // interaction ~ normal(0, 1);

  // individual parameters
  alpha_pr ~ normal(0, 1);
  gamma_pr ~ normal(0, 1);
  for(i in 1:N)
    initV_pr[i] ~ normal(0, 1);

  // subject loop and trial loop
  for (i in 1:N) {
    real ev_onset;
    row_vector[Ncs] ev_offset;
    real PE;      // prediction error

    ev_onset = mean(initV[i]);
    ev_offset = initV[i];

    for (t in 1:T) {
      // onset
      PE = (gamma[i] * mean(ev_offset) - ev_onset);
      relief[i, t, 1] ~ normal(tau * PE, sigma);
      ev_onset += alpha[i] * PE;

      // offset
      PE = (reward[cs[i, t]] - ev_offset[cs[i, t]]);
      relief[i, t, 2] ~ normal(tau * PE, sigma);
      ev_offset += alpha[i] * PE;
    }
  }
}

generated quantities {
  // For group level parameters
  real<lower=0, upper=1> mu_alpha;
  real<lower=0, upper=1> mu_gamma;
  row_vector<lower=0, upper=1>[Ncs] mu_initV;

  // For posterior predictive check
  real y_pred[N, T, O];

  mu_alpha   = Phi_approx(mu_alpha_pr);
  mu_gamma = Phi_approx(mu_gamma_pr);
  mu_initV = Phi_approx(mu_initV_pr);

  { // local section, this saves time and space
    for (i in 1:N) {
      real ev_onset;
      row_vector[Ncs] ev_offset;
      real PE;      // prediction error

      // Initialize values
      ev_onset = mean(initV[i]);
      ev_offset = initV[i];

      for (t in 1:T) {
        PE = (gamma[i] * mean(ev_offset) - ev_onset);
        y_pred[i, t, 1] = tau * PE;
        ev_onset += alpha[i] * PE;

        PE = (reward[cs[i, t]] - ev_offset[cs[i, t]]);
        y_pred[i, t, 2] = tau * PE;
        ev_offset += alpha[i] * PE;
      }
    }
  }
}
