library(rstan)

#Normalise data
birth_death_ts <- readRDS("data/birth_death_ts.rds")

birth_death_ts %<>% group_by(county) %>% 
  mutate(mort_rate_normal = (mort_rate - mean(mort_rate))/sd(mort_rate))


# Fit A GP model - RBF KERNEL ---------------------------------------------

#Start with just Stockholm
stockholm_ts <- birth_death_ts %>% filter(county == "Stockholm")

stan_data <- list(
  N = nrow(stockholm_ts),
  x = stockholm_ts$year,
  y = stockholm_ts$mort_rate_normal
)

