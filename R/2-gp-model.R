source("R/0-setup.R")

#Normalise data
birth_death_ts <- readRDS("data/birth_death_ts.rds")


# Fit A GP model - RBF KERNEL ---------------------------------------------

#Start with just Stockholm
stockholm_ts <- birth_death_ts %>% filter(county == "Stockholm")

# Not conditioned on data -------------------------------------------------

stan_data <- list(
  N = nrow(stockholm_ts),
  x = stockholm_ts$year,
  y = stockholm_ts$mort_rate_normal
)

fit <- stan(file = "stan/method2.stan", data = stan_data, chains = 1, iter = 2000)
params <- extract(fit)
pairs(fit)

mean(params$rho)
mean(params$alpha)
mean(params$sigma)


# Condition on data -------------------------------------------------------

x_predict <- c(stan_data$x
              , (tail(stockholm_ts$year,1) + 1):(tail(stockholm_ts$year,1) + 5))
N_predict <- length(x_predict)
N_obs <- length(stockholm_ts$year)

pred_data <- list(N1=nrow(stockholm_ts)
                  , x1=stockholm_ts$year
                  , y1=stockholm_ts$mort_rate_normal
                  , N2=N_predict, x2=x_predict)

pred_fit <- stan(file='stan/method1.stan', data =pred_data, iter=2000, chains=1)

pred_params <- extract(pred_fit)

f_100 <- pred_params$f[1:100,(N_obs+1):(N_obs+N_predict)]
f_100 %<>% as_tibble() %>% rownames_to_column(var = "it")
names(f_100)[2:length(f_100)] <- as.character(x_predict) 

f_100_trans <- f_100 %>% pivot_longer(-it,names_to = "year", values_to = "mort_rate")
f_100_trans %<>% mutate(year = as.numeric(year)
                        , mort_rate = (mort_rate * unique(stockholm_ts$sd_mort) ) + 
                          unique(stockholm_ts$mean_mort)
)

# f_100_trans <- f_100_trans %>% group_by(year) %>% mutate(estimate = mean(mort_rate))
# everything up to year becomes a function parameterised by: region, # iter, chains
# remove the 100 subset


# Next Steps --------------------------------------------------------------
# Plot - Confidence interval with estimate for each reagion
# Plot - estimate for all regions on one graph

# 1 latent variable common to all regions

