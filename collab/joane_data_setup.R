# Set up -----------------------------------------------------------------------
library(devtools)
install_github("johnrbryant/bdefdata")
library(bdefdata)
deaths <- bdefdata::sweden_deaths
births <- bdefdata::sweden_births
# save data
saveRDS(deaths, "./data/deaths.rds")
saveRDS(births, "./data/births.rds")

#-------------------------------------------------------------------------------
