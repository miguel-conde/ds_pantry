
data {
  int<lower=0> N; // Nº de muestras
  int<lower=0> G; // Nº de grupos (cohortes)
  int<lower=0> g[N]; // grupo
  int<lower=0> t[N]; // time
  real<lower=0> r[N];    // Tasas de retención
}

parameters {
  
  real<lower=0> mu_beta[G];
  real<lower=0> mu_alpha[G];
  
  real<lower=0> beta_normalised[G];
  real<lower=0> alpha_normalised[G];
  
  real<lower=0> sigma_beta[G];
  real<lower=0> sigma_alpha[G];
}

transformed parameters {
  
  real mu_r[N];
  real<lower=0> betas[G];
  real<lower=0> alphas[G];
  
  for (group in 1:G) {
    betas[group] = mu_beta[group] + sigma_beta[group] * beta_normalised[group];
    alphas[group] = mu_alpha[group] + sigma_alpha[group] * alpha_normalised[group];
  }
  
  for (i in 1:N) {
    mu_r[i] = (betas[g[i]] + t[i] - 2) / (alphas[g[i]] + betas[g[i]] + t[i] - 1);
  }
  
}


model {
  
  for (group in 1:G) {
    mu_beta[group] ~ uniform(0.0001, 1000);
    mu_alpha[group] ~ uniform(0.0001, 1000);
    
    beta_normalised[group] ~ normal(0, 1);
    alpha_normalised[group] ~ normal(0, 1);
    
    // sigma_beta[group] ~ exponential(1);
    // sigma_alpha[group] ~ exponential(1);
    sigma_beta[group] ~ uniform(0.0001, 100);
    sigma_alpha[group] ~ uniform(0.0001, 100);
  }
  
  r ~ normal(mu_r, 0.0001);
}


generated quantities {
  
  real<lower=0> r_sim[N];
  real mu_r_sim[N];
  
  for (i in 1:N) {
    mu_r_sim[i] = (betas[g[i]] + t[i] - 2) / (alphas[g[i]] + betas[g[i]] + t[i] - 1);
  }
  
  r_sim = normal_rng(mu_r_sim, 0.0001);
}
