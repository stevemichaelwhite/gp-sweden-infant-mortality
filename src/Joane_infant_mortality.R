#' joane_infant_mortality.R
#' fitting a binomial model with a logistic regression and structured prior

library(dplyr)
library(cmdstanr)

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

# reformat
deaths_df <- as.data.frame(as.table(deaths))
births_df <- as.data.frame(as.table(births))
data <- full_join(deaths_df, births_df, by=c("county","time")) %>%
  rename(deaths = Freq.x, births = Freq.y) %>%
  mutate(county = as.character(county)) %>%
  mutate(county_code = dense_rank(county))

# 