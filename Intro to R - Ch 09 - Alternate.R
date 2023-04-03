############################################################
# Alternate R script to accompany Intro to R for Business, #
# Chapter 09, written by Troy Adair                        #
############################################################
# First, clear memory and the Console 
rm(list=ls(all=TRUE))
cat("\014")

library(here)
library(tidyverse)

# Load the previously referenced data frame in "YT_Sample_Validated.RData"
Scorecard <- read_csv(here("Data","most-recent-cohorts-all-data-elements-1.zip"))

str(Scorecard)

attach(Scorecard)

# Statistics on 1 numerical variable

summary(HIGHDEG)

mean(HIGHDEG)

median(HIGHDEG)

max(HIGHDEG)

min(HIGHDEG)

sum(HIGHDEG)

sd(HIGHDEG)

var(HIGHDEG)

# Statistics on 1 categorical variable

table(HIGHDEG)

n <- length(Scorecard$HIGHDEG)

for(i in 1:n) {
  if(Scorecard$HIGHDEG[i]==0L) {
    Scorecard$HDEGREE[i] <- "0 - Non-Degree"
  } else if(Scorecard$HIGHDEG[i]==1L) {
    Scorecard$HDEGREE[i] <- "1 - Certificate"
  } else if(Scorecard$HIGHDEG[i]==2L) {
    Scorecard$HDEGREE[i] <- "2 - Associate's"
  } else if(Scorecard$HIGHDEG[i]==3L) {
    Scorecard$HDEGREE[i] <- "3 - Bachelor's"
  } else if(Scorecard$HIGHDEG[i]==4L) {
    Scorecard$HDEGREE[i] <- "4 - Graduate"
  } else{}
}

head(HDEGREE,10)

attach(Scorecard)

head(HDEGREE,10)

table(HDEGREE)

# Statistics on 2 categorical variables

table(HDEGREE, CONTROL)

for(i in 1:n) {
  if(CONTROL[i]==1L) {
    Scorecard$ITYPE[i] <- "Public"
  } else if(CONTROL[i]==2L) {
    Scorecard$ITYPE[i] <- "Private Non-Profit"
  } else if(CONTROL[i]==3L) {
    Scorecard$ITYPE[i] <- "Private For-Profit"
  } else{}
}

attach(Scorecard)

table(HDEGREE,ITYPE)

# Statistics on 1 categorical and 1 numerical variable

by(ADM_RATE,ITYPE,summary)

str(Scorecard$ADM_RATE)

Scorecard$ADM_RATE <- as.numeric(Scorecard$ADM_RATE)

str(Scorecard$ADM_RATE)

print(str(Scorecard$ADM_RATE,digits=4))

by(ADM_RATE,ITYPE,summary)

by(Scorecard$ADM_RATE,ITYPE,summary)

attach(Scorecard)

by(ADM_RATE,ITYPE,summary)

by(ADM_RATE,ITYPE,mean)

by(ADM_RATE,ITYPE,mean,na.rm=TRUE)

by(ADM_RATE,ITYPE,sd,na.rm=TRUE)


# Statistics for 2 numerical values (i.e., simple linear regression, AKA "OLS")

cor(PCIP27,SATMTMID)

str(PCIP27)
str(SATMTMID)

Scorecard$PCIP27 <- as.numeric(Scorecard$PCIP27)
Scorecard$SATMTMID <- as.numeric(Scorecard$SATMTMID)

attach(Scorecard)

cor(PCIP27,SATMTMID)

cor(PCIP27,SATMTMID,use="complete.obs")

cov(PCIP27,SATMTMID,use="complete.obs")

OLS <- lm(SATMTMID~PCIP27)

OLS

summary(OLS)

