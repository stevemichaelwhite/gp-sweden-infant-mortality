// binomial model with autoregressive structured prior for years and multilevel
// structure for regions


data {
  int<lower=0> n;             // number of data points
  int<lower=0> D[n];          // deaths
  int<lower=0> B[n];          // births
  vector[n] y;                // year
  vector[n] c;                // county code
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

}

