source("R/0-setup.R")

#Normalise data
birth_death_ts <- readRDS("data/birth_death_ts.rds")

birth_death_ts %<>% group_by(county) %>% 
  mutate(mort_rate_normal = (mort_rate - mean(mort_rate))/sd(mort_rate))


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
pairs(pred_fit)

yr <- c(-3,3)
xr <- c(head(x_predict,1), tail(x_predict,1))
plot(xr, yr, ty='n')
for( i in 1:100){
  lines(x_predict, pred_params$f[i,(N_obs+1):(N_obs+N_predict)], col=rgb(0,0,0,0.1))
}
points(stockholm_ts$year, stockholm_ts$mort_rate_normal, pch=20, col='orange', cex=1)

lines(x_predict, colMeans(pred_params$f[,(N_obs+1):(N_obs+N_predict)]),lwd=3)

# Un-normalised plot ------------------------------------------------------

# Show how additional observations add constraint to fit----------------------



# Next Steps --------------------------------------------------------------
# Analytical from posterior?

