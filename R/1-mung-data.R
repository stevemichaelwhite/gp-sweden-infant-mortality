

library(tidyverse)
library(magrittr)
library(bdefdata)
library(ggplot)
# library(fable)
library(tsibble)
library(feasts)

mort_exp <-3

# Our aim is to estimate Swedish infant mortality rate across regions and time (10-year time period). 
# Modelling allows us to ask questions such as “Are regions struggling more than others to keep babies alive?” or “Is infant survival improving over time?”.

births <- bdefdata::sweden_births
deaths <- bdefdata::sweden_deaths

# data to long form - tsibble
births_tb <- births %>% as.table() %>% as_tibble() %>% rename(year=time, births=n)
deaths_tb <- deaths %>% as.table() %>% as_tibble() %>% rename(year=time, deaths=n)

birth_death_tb <- inner_join(births_tb, deaths_tb, by=c("county","year"))
rm(births, deaths, births_tb, deaths_tb)

# infant mortality rate = deaths among children / live births * 10n
birth_death_tb %<>% mutate(mort_rate = deaths / births * (10^mort_exp))

# tsibble

birth_death_ts <- birth_death_tb %>% mutate(year = as.numeric(year)) %>% 
  as_tsibble(key = county, index = year)
rm(birth_death_tb)

# plots
res_plots[["births"]] <- birth_death_ts %>% autoplot(births)
res_plots[["deaths"]] <- birth_death_ts %>% autoplot(deaths)
res_plots[["mort_rate"]] <- birth_death_ts %>% autoplot(mort_rate)




# Save Data ---------------------------------------------------------------
saveRDS(birth_death_ts, "data/birth_death_ts.rds")
saveRDS(res_plots, "data/res_plots.rds")

