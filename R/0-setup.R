library(rstan)
library(tidyverse)
library(magrittr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()/2)
