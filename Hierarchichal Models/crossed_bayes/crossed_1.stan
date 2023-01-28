
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> n;
  vector[n] Y;
  
  int<lower=1> num_preds;
  
  row_vector[num_preds] X[n];

  int<lower=1> N_marital;
  int marital[n];
  
  int<lower=1> N_sex;
  int sex[n];
}

transformed data {
  
  real avg_Y;
  real sd_Y;
  
  row_vector[num_preds] X_marital_avgs;
  row_vector[num_preds] X_marital_sds;
  
  row_vector[num_preds] X_sex_avgs;
  row_vector[num_preds] X_sex_sds;
  
  avg_Y = mean(Y);
  sd_Y  = sd(Y);
  
  for (i in 1:num_preds) {
    X_marital_sds[i]  =   sd(X[, i]);
    X_sex_sds[i]      =   sd(X[, i]);
  }
}

parameters {
  real<lower=0> sigma;
  
  // 1 vector fila de betas por cada marital; 
  row_vector[num_preds] betas_marital[N_marital];
  
  // 1 vector fila de betas por cada sex; 
  row_vector[num_preds] betas_sex[N_sex];
  
  vector[num_preds] bar_beta_marital_0;
  vector<lower=0>[num_preds] sigma_beta_marital_0;
  
  vector[num_preds] bar_beta_sex_0;
  vector<lower=0>[num_preds] sigma_beta_sex_0;
}

transformed parameters {
  vector[n] mu;
  vector[n] mu_marital;
  vector[n] mu_sex;
  
  // vector[num_preds] bar_beta_marital;
  // // vector<lower=0>[num_preds] sigma_beta_marital;
  // 
  // vector[num_preds] bar_beta_sex;
  // // vector<lower=0>[num_preds] sigma_beta_sex;
  // 
  // // print("TRANSFORMED PARAMETERS - sd_Y: ", sd_Y);   
  // for ( i in 1:N_marital) {
  //   // print("TRANSFORMED PARAMETERS - X_marital_sds[", i, "]:", X_marital_sds[i]);
  //   // print("TRANSFORMED PARAMETERS - bar_beta_marital_0[", i, "]: ", bar_beta_marital_0[i]);
  //   bar_beta_marital[i]  = sd_Y / X_marital_sds[i] * bar_beta_marital_0[i];
  //   
  //   // print("TRANSFORMED PARAMETERS - bar_beta_marital[", i, "]: ", bar_beta_marital[i]);
  // }
  // 
  // for ( i in 1:N_sex) {
  //   // print("TRANSFORMED PARAMETERS - X_sex_sds[", i, "]:", X_sex_sds[i]);
  //   // print("TRANSFORMED PARAMETERS - bar_beta_sex_0[", i, "]: ", bar_beta_sex_0[i]);
  //   bar_beta_sex[i]  = sd_Y / X_sex_sds[i] * bar_beta_sex_0[i];
  //   
  //   // print("TRANSFORMED PARAMETERS - bar_beta_sex[", i, "]: ", bar_beta_sex[i]);
  // }

  
  for (i in 1:n) {
     mu_marital[i] = dot_product(X[i], betas_marital[marital[i], ]);
     mu_sex[i]     = dot_product(X[i], betas_sex[sex[i], ]);
     
     mu[i]         = mu_marital[i] + mu_sex[i];
  }
}

model {
  
  // HYPERPRIORS
  
  // for (i in 1:num_preds) {
  //     bar_beta_marital_0[i] ~ normal(0, 1);
  //     sigma_beta_marital_0[i] ~ exponential(1);
  //     bar_beta_sex_0[i] ~ normal(0, 1);
  //     sigma_beta_sex_0[i] ~ exponential(1);
  // }
  
  // PRIORS
  
  sigma ~ exponential(10);
  
  for (m in 1:N_marital) {
    for (i in 1:num_preds) {
      // print("MODEL - bar_beta_marital[", i, "]: ", bar_beta_marital[r]);
      // print("MODEL - sigma_beta_marital_0[", i, "]: ", sigma_beta_marital_0[r]);
      // betas_marital[r, i] ~ normal(bar_beta_marital[r], sigma_beta_marital_0[r]);
      betas_marital[m, i] ~ normal(0, 1);
      // print("MODEL - betas_marital[", r, ", ", i, "]: ", betas_marital[r, i]);
      // print("MODEL - betas_marital[", m, ", ", i, "] = ", betas_marital[m, i])
    }
  }
  
  for (s in 1:N_sex) {
    for (i in 1:num_preds) {
      // print("MODEL - bar_beta_sex[", i, "]: ", bar_beta_sex[r]);
      // print("MODEL - sigma_beta_sex_0[", i, "]: ", sigma_beta_sex_0[r]);
      // betas_sex[r, i] ~ normal(bar_beta_sex[r], sigma_beta_sex_0[r]);
      betas_sex[s, i] ~ normal(0, 1);
      // print("MODEL - betas_sex[", r, ", ", i, "]: ", betas_sex[r, i]);
      // print("MODEL - betas_sex[", s, ", ", i, "] = ", betas_marital[s, i])
    }
  }
  
  // MODEL
  
  Y ~ normal(mu, sigma);
  
}

