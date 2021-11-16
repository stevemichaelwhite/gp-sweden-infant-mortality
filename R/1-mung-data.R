# devtools::install_github("johnrbryant/bdefdata")
# install.packages("fable")

library(tidyverse)
library(magrittr)
library(bdefdata)
library(ggplot)
# library(fable)
library(tsibble)
library(feasts)

# save plots and tables for rmarkdown
res_plots <- list()
res_tables <- list()


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

birth_death_ts %<>% group_by(county) %>% 
  mutate(mean_mort = mean(mort_rate),
         sd_mort = sd(mort_rate),
         mort_rate_normal = (mort_rate - mean_mort)/sd_mort)

# plots
res_plots[["births"]] <- birth_death_ts %>% autoplot(births)
res_plots[["deaths"]] <- birth_death_ts %>% autoplot(deaths)

birth_death_ts %>% as_tibble() %>% group_by(county) %>%
  summarise(sum_births = sum(births))  %>% arrange(desc(sum_births) ) 

graph_region_dat <- birth_death_ts  %>% 
  group_by(county) %>% 
  mutate(sum_births = sum(births)) %>% ungroup() %>%
  mutate(qlabel=sprintf("%s (tot_births=%s)",county, round(sum_births,2))) %>% 
  mutate(qlabel = fct_reorder(as.factor(qlabel), -sum_births))

res_plots[["mort_rate"]] <- graph_region_dat %>% ggplot(aes(x=year, y=mort_rate)) + geom_line() + geom_point() +
  facet_wrap(facets = vars(qlabel),scales = "free", ncol = 3)

# Save Data ---------------------------------------------------------------
saveRDS(birth_death_ts, "data/birth_death_ts.rds")
saveRDS(res_plots, "data/res_plots.rds")

