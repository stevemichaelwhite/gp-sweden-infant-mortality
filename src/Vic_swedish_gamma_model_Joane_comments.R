load(file="reg_df.rda") # The rati variable is in the 00's, as I used deaths per 10,000
head(reg_df) # A-G are numbers having max eduction (see text file)
#
# Gamma regression does not like 0's, so do a fiddle:
reg_df_pos <-  reg_df # Take a copy (shallow at this stage?)
reg_df_pos$ratio <- reg_df_pos$ratio + 1 # Adding 1 is ok as numbers are per 10000

########
# JOANE: out of curiosity, why adding 1 to the ratio?
########

library(brms)
# Fit with -1 in regression model so that county coefficients are ALL calculated
# I have not use year in the regression, so you might try that
brm_glm_reg <- brm(ratio ~ -1 + county, data=reg_df_pos, family=Gamma(link="log"),
                   prior=c( # prior(normal(0,2),class="Intercept"),
                     prior(normal(0,2),class="b"),
                     prior(gamma(0.01,0.01),class="shape")),
                   chains=4,iter=1000, cores=4)
########
# JOANE: you may as well choose the same number of chains as cores, as by default
# there is no within-chain parallelisation in brms
########

summary(brm_glm_reg)
print(brm_glm_reg, prior=T)
#
# plot(marginal_effects(brm_glm_reg),points=T)
#
## extract fitted values
fitted_values <- fitted(brm_glm_reg)
dim(fitted_values) # 441   4 (21 regions, 21 time periods - confusing huh!!!)
fixef(brm_glm_reg)
#
## plot fitted means against actual response
library(ggplot2)
dat <- as.data.frame(cbind(Y = standata(brm_glm_reg)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))

##########
# JOANE: You can also do it this way:
library(bayesplot)
y_obs <- reg_df_pos$ratio
y_rep <- posterior_predict(brm_glm_reg, draws = 1000)
ppc_stat_grouped(y_obs, y_rep, group = reg_df_pos$ratio)
#########

#########
#JOANE: looking at different countries effects and shape estimate
posterior <- as.array(brm_glm_reg)
b_names <- parnames(brm_glm_reg)[grep("b", parnames(brm_glm_reg))]
mcmc_intervals(posterior, pars=b_names)
#or
mcmc_areas(posterior, pars=b_names)
mcmc_areas(posterior, pars="shape")
#########


# Try some hypotheses that the intercepts for differnt regions are the same
# https://paul-buerkner.github.io/brms/reference/hypothesis.html
parnames(brm_glm_reg) # They have a b_ prefix
hyp <- "b_countyGotland - b_countyJamtland  = 0" # Counties that are most different
ans <-hypothesis(brm_glm_reg, hyp, class = NULL)
print(ans)
# str(ans)
plot(ans$samples$H1)
plot(density(ans$samples$H1)) # Indicates difference is not 0
#

