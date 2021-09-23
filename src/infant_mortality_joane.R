#' infant_mortality_joane.R

# Set up
library(devtools)
install_github("johnrbryant/bdefdata")
library(bdefdata)
deaths <- bdefdata::sweden_deaths
births <- bdefdata::sweden_births