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

i <- sapply(deaths_melted, is.factor)
deaths_melted[i] <- lapply(deaths_melted[i], as.numeric)

j <- sapply(births_melted, is.factor)
births_melted[j] <- lapply(births_melted[j], as.numeric)

data.table::setDT(deaths_melted)
data.table::setDT(births_melted)

setnames(deaths_melted,c("county","time","deaths"))
setnames(births_melted,c("county","time","births"))

infant_dataset<-merge(births_melted,deaths_melted,by=c("county","time"))

infant_dataset[, `:=` (
  delta = ifelse(time %in% "Stockholm", 1, 0),
  county = factor(county),
  t = time - min(time) + 1)]

infant_dataset[,-c("delta")]
infant_dataset[order(county)]
infant_dataset[,infant_mortality_rate:=1000*(deaths/births)]

fit1 <-
  brms::brm(
    infant_mortality_rate~ t + county,
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


fit2 <-
  brms::brm(
    infant_mortality_rate~ t + county,
    data = infant_dataset,
    family = gamma(link=log),
    warmup = 500, 
    iter = 2000,
    chains=2,
    cores=2,
    seed = 123
  )

summary(fit2)
prior_summary(fit2)
fit2<-add_criterion(fit2,"loo")

loo_compare(fit1,fit2,criterion="loo")
