devtools::install_github("johnrbryant/bdefdata")

# Our aim is to estimate Swedish infant mortality rate across regions and time (10-year time period). 
# Modelling allows us to ask questions such as “Are regions struggling more than others to keep babies alive?” or “Is infant survival improving over time?”.

library(bdefdata)
deaths <- bdefdata::sweden_deaths
births <- bdefdata::sweden_births

# data to long form - tsibble