data {
  int<lower=1> N1; //number data observed
  real x1[N1];
  vector[N1] y1;
  int<lower=1> N2; //number to be predicted
  real x2[N2];
}

transformed data{
  int<lower=1> N = N1 + N2;
  real x[N]; //number all
  for (n1 in 1:N1) x[n1] = x1[n1];
  for (n2 in 1:N2) x[N1 + n2] = x2[n2];
}

parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  // For more efficient sampling
  vector[N] eta;
}

transformed parameters {
  // Not a function but are test points
  vector[N] f;
  {
  // The RBF function + small constant added to diagnal to ensure positive definite.
  matrix[N,N] K = cov_exp_quad(x,alpha,rho) + diag_matrix(rep_vector(1e-9, N));
  // Lower triangular is more efficient and stable to deal with.
  matrix[N,N] L_K = cholesky_decompose(K);
  // Non-centered parameterisation is more efficient to run
  f = L_K * eta;
  }
}

model {
  // Wide prior on frequency
  rho ~ normal(0,3);
  alpha ~ normal(0,1);
  sigma ~ normal(0,1);
  eta ~ normal(0,1);
  //Liklihood
  y1 ~ normal(f[1:N1], sigma);
}

generated quantities {
  // Predicted values
  vector[N2] y2;
  for(n2 in 1:N2)
    y2[n2] = normal_rng(f[N1 + n2], sigma);
}
