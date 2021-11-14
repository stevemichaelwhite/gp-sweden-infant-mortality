load(file="reg_df.rda") # The rati variable is in the 00's, as I used deaths per 10,000
head(reg_df) # A-G are numbers having max eduction (see text file)
#
# Gamma regression does not like 0's, so do a fiddle:
reg_df_pos <-  reg_df # Take a copy (shallow at this stage?)
reg_df_pos$ratio <- reg_df_pos$ratio + 1 # Adding 1 is ok as numbers are per 10000
#
library(brms)
# Fit with -1 in regression model so that county coefficients are ALL calculated
# I have not use year in the regression, so you might try that
brm_glm_reg <- brm(ratio ~ -1 + county, data=reg_df_pos, family=Gamma(link="log"),
                   prior=c( # prior(normal(0,2),class="Intercept"),
                     prior(normal(0,2),class="b"),
                     prior(gamma(0.01,0.01),class="shape")),
                   chains=2,iter=1000, cores=4)
#
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
#
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

