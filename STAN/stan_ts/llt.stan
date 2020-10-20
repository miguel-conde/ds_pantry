
data {
  int<lower=0> N;
  vector[N] y;
}


parameters {
  real<lower=0> epsilon;
  real<lower=0> eta_lvl;
  real<lower=0> eta_slp;
}

transformed parameters {
  real level[N];
  real slope[N];
  
  level[1] = y[1];
  slope[1] = 0;
  
  for(i in 2:N) {
    level[i] = level[i-1] + slope[i-1] + eta_lvl;
    slope[i] = slope[i-1] + eta_slp;
  }
}


model {
  y[2:N] ~ normal(level[2:N], epsilon);
  epsilon ~ cauchy(0, 5);
  eta_lvl ~ normal(0,1);
  eta_slp ~ normal(0,1);
}

