// binomial model with simple hierarchical priors for years and regions.

data {
  int<lower=0> N;                       // number of data points
  int<lower=0> Nga;                     // number of year groups
  int<lower=0> Ngc;                     // number of county groups
  int<lower=0> y[N];                    // deaths
  int<lower=0> n[N];                    // births
  int<lower=0, upper=Nga> ga[N];        // vector of year codes
  int<lower=0, upper=Ngc> gc[N];        // vector of county codes
}


parameters {
  real mu;                              // global intercept
  vector[Nga] offset_a_raw;             // year effects (raw)
  vector[Ngc] offset_c_raw;             // county effects (raw)
  real<lower=0> sigma_a;                // year standard deviation
  real<lower=0> sigma_c;                // county standard deviation
}


transformed parameters {
  vector[Nga] offset_a;                 // year effects
  vector[Ngc] offset_c;                 // county effects
  
  offset_a = sigma_a * offset_a_raw;
  offset_c = sigma_c * offset_c_raw;
}


model {
  vector[N] mu_vec;
  vector[N] logit_theta;
  
  // logistic regression
  mu_vec = rep_vector(mu, N);
  logit_theta = mu_vec + offset_a[ga] + offset_c[gc];
  
  // binomial data generation
  target += binomial_logit_lpmf(y | n, logit_theta);
  
  // priors
  mu ~ normal(-5,5);                   // 20th century mortality is low
  sigma_a ~ cauchy(0,2.5);
  sigma_c ~ cauchy(0,2.5);
  offset_a_raw ~ normal(0,1);
  offset_c_raw ~ normal(0,1);
}

