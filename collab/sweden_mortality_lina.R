
# clone the project before pushing data 
#pwd
#cd /home/STATSNZ/lberbesi//Network-Shares/U-Drive-SAS-02BAU/StdsMthds/"Statistical Methods"/Users/Lina/bayes_practical_problems/sweden_mortality/sweden_infant_mortality
# git clone https://gitlabstats-prd/StatsMethods/bayesian-cop/projects/sweden_infant_mortality.git
# git --init --initial-branch=main
# git remote add origin https://gitlabstats-prd/StatsMethods/bayesian-cop/projects/sweden_infant_mortality/-/tree/master/src
# git add sweden_mortality_lina.R
# git commit  -n "Initial commit"
# git push -u origin main

library("devtools")
library("bdefdata")
suppressPackageStartupMessages(library("brms",quietly=TRUE))
suppressPackageStartupMessages(library("reshape2",quietly=TRUE))
library("data.table",quietly=TRUE)

install_github("johnrbryant/bdefdata")

cols<-c(as.character(seq(1995,2015,1)))

deaths<-data.frame(bdefdata::sweden_deaths)
names(deaths)<-cols
deaths$county<-row.names(deaths)
row.names(deaths)<-1:nrow(deaths)
deaths[,c(22,1:21)]

births<-data.frame(bdefdata::sweden_births)
names(births)<-cols
births$county<-row.names(births)
row.names(births)<-1:nrow(births)
births[,c(22,1:21)]

deaths_melted<-reshape2::melt(deaths,id="county")
names(deaths_melted)<-c("county","time","value")
births_melted<-reshape2::melt(births,id="county")
names(births_melted)<-c("county","time","value")

data.table::setDT(deaths_melted)
data.table::setDT(births_melted)

setnames(deaths_melted,c("county","time","deaths"))
setnames(births_melted,c("county","time","births"))

infant_dataset<-merge(births_melted,deaths_melted,by=c("county","time"))

infant_dataset[, `:=` (
  delta = ifelse(time %in% "Stockholm", 1, 0),
  county = factor(county),
  t = as.numeric(time) - min(as.numeric(time)) + 1)]

infant_dataset[,delta:=NULL]
infant_dataset[order(county)]
infant_dataset[,infant_mortality_rate:=1000*(deaths/births)]

fit1 <-
  brms::brm(
    infant_mortality_rate~ (1|t) + (1|county),
    data = infant_dataset,
    family = gaussian(),
    warmup = 500, 
    iter = 2000,
    chains=2,
    cores=2,
    seed = 123
  )

summary(fit1)
prior_summary(fit1)
fit1<-add_criterion(fit1,"loo")


fit3 <-
  brms::brm(
    deaths|trials(births)~ 1 + (1|time) + (1|county), # deaths=number of target events | total number of trials
    data = infant_dataset,
    family = binomial(link=logit),
    warmup = 500, 
    iter = 2000,
    chains=2,
    cores=2,
    seed = 123
  )

summary(fit3)
prior_summary(fit3)

fit3<-add_criterion(fit3,"loo")


fit4<-glm(cbind(deaths,births-deaths)~1+time+county, # How to do it in the frequentist way?
          family=binomial(logit),
          data=infant_dataset)

summary(fit4)



