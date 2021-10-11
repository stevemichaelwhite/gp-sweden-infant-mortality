#' joane_infant_mortality.R
#' fitting a binomial model with a logistic regression 
#' and simple/structured hierarchical prior

library(dplyr)
library(cmdstanr) #instead of rstan
library(bayesplot)

deaths <- readRDS("./data/deaths.rds")
births <- readRDS("./data/births.rds")
class(deaths)
dim(births)
dim(deaths)

png("./out/distribution of births and deaths.png",height=600,width=450)
par(mfrow=c(2,1))
plot(density(births), col="blue", main = "births")
plot(density(deaths), col="red", main = "deaths")
dev.off()

# assume everyone is born on Jan 1st?
obs_mortality <- deaths/births
par(mfrow=c(1,1))
hist(obs_mortality, main = "mortality" ,col="grey")

# reformat to 1 long dataframe
deaths_df <- as.data.frame(as.table(deaths))
births_df <- as.data.frame(as.table(births))
data <- full_join(deaths_df, births_df, by=c("county","time")) %>%
  rename(deaths = Freq.x, births = Freq.y) %>%
  mutate(county = as.character(county)) %>%
  mutate(county_code = dense_rank(county)) %>%
  mutate(time_code = dense_rank(time))

head(data)


#-------------------------------------------------------------------------------
# Simple prior
#-------------------------------------------------------------------------------

# model setup : data
stan_data <- list(N   = nrow(data),
                  Nga = length(unique(data$time_code)),
                  Ngc = length(unique(data$county_code)),
                  y   = data$deaths,
                  n   = data$births,
                  ga  = data$time_code,
                  gc  = data$county_code)

# model setup : compiling the Stan model
stan_model <- cmdstan_model("./src/joane_stan_model_mlr_simple_hierarchical_prior.stan")


# run the model
nchains <- 4
options(mc.cores = nchains)

fit1 <- stan_model$sample(data = stan_data,
                          seed = 8534,
                          chains = nchains,
                          parallel_chains = nchains,
                          iter_warmup = 1000,
                          iter_sampling = 1000,
                          refresh = 100)
options(mc.cores = 1)

# fit summary
fit_summary1 <- fit1$summary(variables=c("mu","offset_a", "offset_c",
                                         "sigma_a", "sigma_c"))
# chain diagnostics
par(mfrow = c(1,2))
hist(fit_summary1$rhat, col="grey", main = "Rhat")
hist(fit_summary1$ess_bulk, col="grey", main = "ESS")

fit_summary1[which(fit_summary1$ess_bulk < 1000),]

draws1 <- fit1$draws()
mcmc_trace(draws1, pars=c("mu"))
mcmc_trace(draws1, pars=c("offset_c_raw[3]"))

# plot offsets
mcmc_intervals(draws1, 
               pars = paste0("offset_a[",
                             1:length(unique(data$time_code)),
                             "]"))

mcmc_intervals(draws1, 
               pars = paste0("offset_c[",
                             1:length(unique(data$county_code)),
                             "]"))
#plot hyperparameters sigma
mcmc_intervals(draws1, 
               pars = c("sigma_a", "sigma_c"))

#Look at predictions
yrep1 <- fit1$draws(variable = "y_pred", format = "draws_matrix")

ppc_stat_grouped(y=data$deaths,yrep=yrep1,group=data$county)
ppc_stat_grouped(y=data$deaths,yrep=yrep1,group=data$time)
ppc_intervals_grouped(y=data$deaths,yrep=yrep1,group=data$county)

# PSIS-LOO
fit1$loo()


#-------------------------------------------------------------------------------
# Structured prior
#-------------------------------------------------------------------------------
