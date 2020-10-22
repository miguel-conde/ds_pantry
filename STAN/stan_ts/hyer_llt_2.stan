data {
  
  int<lower=0> n;         // Number of observations
  int<lower=0> n_country; // Number of countries
  vector[n] y;            // Response
  int S[n_country];       // sample sizes of each country
  int index[n_country];   // start position of each country
  
}

transformed data {
  
}


parameters {
  real eta_slp[n]; // Slope innovation
  real eta_lvl[n]; // Level innovation
  
  real epsilon[n_country];
  real<lower = 0> sigma_lvl[n_country];
  real<lower = 0> sigma_slp[n_country];
  
  real eta_slp_top; 
  real<lower = 0> sigma_eta_slp_top;
  real eta_lvl_top; 
  real<lower = 0> sigma_eta_lvl_top;
  
  real epsilon_top;
  real<lower = 0> sigma_epsilon_top;
  real <lower = 0>sigma_lvl_top;
  real<lower = 0> sigma_sigma_lvl_top;
  real <lower = 0>sigma_slp_top;
  real<lower = 0> sigma_sigma_slp_top;
}

transformed parameters {
  
  vector[n]  level;
  vector[n]  slope;
  
  for(i in 1:n_country) {
    
    level[index[i]] = sigma_lvl[i];
    slope[index[i]] = sigma_slp[i];
    
    for(j in 2:S[i]) {
      
      level[index[i] + j - 1] = level[index[i] + j - 2] + slope[index[i] + j - 2] + 
                                sigma_lvl[i]*eta_lvl[index[i] + j - 1]; 
      slope[index[i] + j - 1] = slope[index[i] + j - 2] + 
                                sigma_slp[i]*eta_slp[index[i] + j - 1];
    }
  }
}


model {
  
  for(i in 1:n_country) {
    
    y[index[i]:(index[i]+S[i]-1)] ~ normal(level[index[i]:(index[i]+S[i]-1)], epsilon[i]);
    print(epsilon[i])
    
    eta_slp[index[i]:index[i]+S[i]-1] ~ normal(eta_slp_top, sigma_eta_slp_top);
    eta_lvl[index[i]:index[i]+S[i]-1] ~ normal(eta_lvl_top, sigma_eta_lvl_top);
    
    epsilon[i] ~ normal(epsilon_top, sigma_epsilon_top);
    sigma_lvl[i] ~ gamma(sigma_lvl_top, sigma_sigma_lvl_top);
    sigma_slp[i] ~ gamma(sigma_slp_top, sigma_sigma_slp_top);
    
    eta_slp_top ~ normal(0, 1); 
    sigma_eta_slp_top ~ gamma(1, 1);
    eta_lvl_top ~ normal(0, 1); 
    sigma_eta_lvl_top ~ gamma(1, 1);
    
    epsilon_top ~ normal(0, 1);
    sigma_epsilon_top ~ gamma(1, 1);
    sigma_lvl_top ~ gamma(1, 1);
    sigma_sigma_lvl_top ~ gamma(1, 1);
    sigma_slp_top ~ gamma(1, 1);
    sigma_sigma_slp_top ~ gamma(1, 1);
  }
  
  
}

generated quantities {

  real epsilon_average;
  real<lower=0> sigma_lvl_average;
  real<lower=0> sigma_slp_average;

  real logLikelihood[n];

  epsilon_average = normal_rng(epsilon_top, sigma_epsilon_top);
  sigma_lvl_average = gamma_rng(sigma_lvl_top, sigma_sigma_lvl_top);
  sigma_slp_average = gamma_rng(sigma_slp_top, sigma_sigma_slp_top);

  for(i in 1:n) {
    logLikelihood[i] = normal_lpdf(y[i] | level[i], epsilon);
  }
}
