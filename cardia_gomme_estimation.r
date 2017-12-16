### For "Market Work, Housework and Childcare: A Time Use
### Approach" by Emanuela Cardia and Paul Gomme, published in Review of
### Economic Dynamics.
###
### To run: source("cardia_gomme_estimation.r")
###
### Required American Time Use Survey data, available from 
### https://www.bls.gov/tus/
### Place data in subdirectories for each year (2003, 2004, etc.)
###
### This code produces the estimates of the childcare production function
### reported in Table 3 in the paper.
###
### This file created by Paul Gomme is licensed under a Creative Commons
### Attribution 4.0 International License:
### https://creativecommons.org/licenses/by/4.0/

rm(list=ls())
sink("cardia_gomme_estimation.output", append=FALSE, split=FALSE)
options(digits=20)
library(lmtest)

NTUS <-  12

activity.file <- c("2003/atusact_2003.dat", "2004/atusact_2004.dat",
              "2005/atusact_2005.dat", "2006/atusact_2006.dat",
              "2007/atusact_2007.dat", "2008/atusact_2008.dat",
              "2009/atusact_2009.dat", "2010/atusact_2010.dat",
              "2011/atusact_2011.dat", "2012/atusact_2012.dat",
              "2013/atusact_2013.dat", "2014/atusact_2014.dat")
respondent.file <- c("2003/atusresp_2003.dat", "2004/atusresp_2004.dat",
                "2005/atusresp_2005.dat", "2006/atusresp_2006.dat",
                "2007/atusresp_2007.dat", "2008/atusresp_2008.dat",
                "2009/atusresp_2009.dat", "2010/atusresp_2010.dat",
                "2011/atusresp_2011.dat", "2012/atusresp_2012.dat",
                "2013/atusresp_2013.dat", "2014/atusresp_2014.dat")
roster.file <- c("2003/atusrost_2003.dat", "2004/atusrost_2004.dat",
                "2005/atusrost_2005.dat", "2006/atusrost_2006.dat",
                "2007/atusrost_2007.dat", "2008/atusrost_2008.dat",
                "2009/atusrost_2009.dat", "2010/atusrost_2010.dat",
                "2011/atusrost_2011.dat", "2012/atusrost_2012.dat",
                "2013/atusrost_2013.dat", "2014/atusrost_2014.dat")
who.file <- c("2003/atuswho_2003.dat", "2004/atuswho_2004.dat",
                "2005/atuswho_2005.dat", "2006/atuswho_2006.dat",
                "2007/atuswho_2007.dat", "2008/atuswho_2008.dat",
                "2009/atuswho_2009.dat", "2010/atuswho_2010.dat",
                "2011/atuswho_2011.dat", "2012/atuswho_2012.dat",
                "2013/atuswho_2013.dat", "2014/atuswho_2014.dat")

atusact <- read.csv(activity.file[1])
atusresp <- read.csv(respondent.file[1])
atusrost <- read.csv(roster.file[1])
atuswho <- read.csv(who.file[1])

atusact.2003 <- read.csv(activity.file[1])
atusact.2004 <- read.csv(activity.file[2])
atusact.2005 <- read.csv(activity.file[3])
atusact.2006 <- read.csv(activity.file[4])
atusact.2007 <- read.csv(activity.file[5])
atusact.2008 <- read.csv(activity.file[6])
atusact.2009 <- read.csv(activity.file[7])
atusact.2010 <- read.csv(activity.file[8])
atusact.2011 <- read.csv(activity.file[9])
atusact.2012 <- read.csv(activity.file[10])
atusact.2013 <- read.csv(activity.file[11])
atusact.2014 <- read.csv(activity.file[12])

common.cols <- intersect(colnames(atusact.2003), colnames(atusact.2004))
common.cols <- intersect(common.cols, colnames(atusact.2005))
common.cols <- intersect(common.cols, colnames(atusact.2006))
common.cols <- intersect(common.cols, colnames(atusact.2007))
common.cols <- intersect(common.cols, colnames(atusact.2008))
common.cols <- intersect(common.cols, colnames(atusact.2009))
common.cols <- intersect(common.cols, colnames(atusact.2010))
common.cols <- intersect(common.cols, colnames(atusact.2011))
common.cols <- intersect(common.cols, colnames(atusact.2012))
common.cols <- intersect(common.cols, colnames(atusact.2013))
common.cols <- intersect(common.cols, colnames(atusact.2014))

atusact <- rbind(subset(atusact.2003, select=common.cols),
                 subset(atusact.2004, select=common.cols),
                 subset(atusact.2005, select=common.cols),
                 subset(atusact.2006, select=common.cols),
                 subset(atusact.2007, select=common.cols),
                 subset(atusact.2008, select=common.cols),
                 subset(atusact.2009, select=common.cols),
                 subset(atusact.2010, select=common.cols),
                 subset(atusact.2011, select=common.cols),
                 subset(atusact.2012, select=common.cols),
                 subset(atusact.2013, select=common.cols),
                 subset(atusact.2014, select=common.cols))

atusresp.2003 <- read.csv(respondent.file[1])
atusresp.2004 <- read.csv(respondent.file[2])
atusresp.2005 <- read.csv(respondent.file[3])
atusresp.2006 <- read.csv(respondent.file[4])
atusresp.2007 <- read.csv(respondent.file[5])
atusresp.2008 <- read.csv(respondent.file[6])
atusresp.2009 <- read.csv(respondent.file[7])
atusresp.2010 <- read.csv(respondent.file[8])
atusresp.2011 <- read.csv(respondent.file[9])
atusresp.2012 <- read.csv(respondent.file[10])
atusresp.2013 <- read.csv(respondent.file[11])
atusresp.2014 <- read.csv(respondent.file[12])

atusresp.2003$TUFINLWGT <- atusresp.2003$TU06FWGT
atusresp.2004$TUFINLWGT <- atusresp.2004$TU06FWGT
atusresp.2005$TUFINLWGT <- atusresp.2005$TU06FWGT

common.cols <- intersect(colnames(atusresp.2003), colnames(atusresp.2004))
common.cols <- intersect(common.cols, colnames(atusresp.2005))
common.cols <- intersect(common.cols, colnames(atusresp.2006))
common.cols <- intersect(common.cols, colnames(atusresp.2007))
common.cols <- intersect(common.cols, colnames(atusresp.2008))
common.cols <- intersect(common.cols, colnames(atusresp.2009))
common.cols <- intersect(common.cols, colnames(atusresp.2010))
common.cols <- intersect(common.cols, colnames(atusresp.2011))
common.cols <- intersect(common.cols, colnames(atusresp.2012))
common.cols <- intersect(common.cols, colnames(atusresp.2013))
common.cols <- intersect(common.cols, colnames(atusresp.2014))

atusresp <- rbind(subset(atusresp.2003, select=common.cols),
                 subset(atusresp.2004, select=common.cols),
                 subset(atusresp.2005, select=common.cols),
                 subset(atusresp.2006, select=common.cols),
                 subset(atusresp.2007, select=common.cols),
                 subset(atusresp.2008, select=common.cols),
                 subset(atusresp.2009, select=common.cols),
                 subset(atusresp.2010, select=common.cols),
                 subset(atusresp.2011, select=common.cols),
                 subset(atusresp.2012, select=common.cols),
                 subset(atusresp.2013, select=common.cols),
                 subset(atusresp.2014, select=common.cols))

atusrost.2003 <- read.csv(roster.file[1])
atusrost.2004 <- read.csv(roster.file[2])
atusrost.2005 <- read.csv(roster.file[3])
atusrost.2006 <- read.csv(roster.file[4])
atusrost.2007 <- read.csv(roster.file[5])
atusrost.2008 <- read.csv(roster.file[6])
atusrost.2009 <- read.csv(roster.file[7])
atusrost.2010 <- read.csv(roster.file[8])
atusrost.2011 <- read.csv(roster.file[9])
atusrost.2012 <- read.csv(roster.file[10])
atusrost.2013 <- read.csv(roster.file[11])
atusrost.2014 <- read.csv(roster.file[12])

common.cols <- intersect(colnames(atusrost.2003), colnames(atusrost.2004))
common.cols <- intersect(common.cols, colnames(atusrost.2005))
common.cols <- intersect(common.cols, colnames(atusrost.2006))
common.cols <- intersect(common.cols, colnames(atusrost.2007))
common.cols <- intersect(common.cols, colnames(atusrost.2008))
common.cols <- intersect(common.cols, colnames(atusrost.2009))
common.cols <- intersect(common.cols, colnames(atusrost.2010))
common.cols <- intersect(common.cols, colnames(atusrost.2011))
common.cols <- intersect(common.cols, colnames(atusrost.2012))
common.cols <- intersect(common.cols, colnames(atusrost.2013))
common.cols <- intersect(common.cols, colnames(atusrost.2014))

atusrost <- rbind(subset(atusrost.2003, select=common.cols),
                 subset(atusrost.2004, select=common.cols),
                 subset(atusrost.2005, select=common.cols),
                 subset(atusrost.2006, select=common.cols),
                 subset(atusrost.2007, select=common.cols),
                 subset(atusrost.2008, select=common.cols),
                 subset(atusrost.2009, select=common.cols),
                 subset(atusrost.2010, select=common.cols),
                 subset(atusrost.2011, select=common.cols),
                 subset(atusrost.2012, select=common.cols),
                 subset(atusrost.2013, select=common.cols),
                 subset(atusrost.2014, select=common.cols))

atuswho.2003 <- read.csv(who.file[1])
atuswho.2004 <- read.csv(who.file[2])
atuswho.2005 <- read.csv(who.file[3])
atuswho.2006 <- read.csv(who.file[4])
atuswho.2007 <- read.csv(who.file[5])
atuswho.2008 <- read.csv(who.file[6])
atuswho.2009 <- read.csv(who.file[7])
atuswho.2010 <- read.csv(who.file[8])
atuswho.2011 <- read.csv(who.file[9])
atuswho.2012 <- read.csv(who.file[10])
atuswho.2013 <- read.csv(who.file[11])
atuswho.2014 <- read.csv(who.file[12])

common.cols <- intersect(colnames(atuswho.2003), colnames(atuswho.2004))
common.cols <- intersect(common.cols, colnames(atuswho.2005))
common.cols <- intersect(common.cols, colnames(atuswho.2006))
common.cols <- intersect(common.cols, colnames(atuswho.2007))
common.cols <- intersect(common.cols, colnames(atuswho.2008))
common.cols <- intersect(common.cols, colnames(atuswho.2009))
common.cols <- intersect(common.cols, colnames(atuswho.2010))
common.cols <- intersect(common.cols, colnames(atuswho.2011))
common.cols <- intersect(common.cols, colnames(atuswho.2012))
common.cols <- intersect(common.cols, colnames(atuswho.2013))
common.cols <- intersect(common.cols, colnames(atuswho.2014))

atuswho <- rbind(subset(atuswho.2003, select=common.cols),
                 subset(atuswho.2004, select=common.cols),
                 subset(atuswho.2005, select=common.cols),
                 subset(atuswho.2006, select=common.cols),
                 subset(atuswho.2007, select=common.cols),
                 subset(atuswho.2008, select=common.cols),
                 subset(atuswho.2009, select=common.cols),
                 subset(atuswho.2010, select=common.cols),
                 subset(atuswho.2011, select=common.cols),
                 subset(atuswho.2012, select=common.cols),
                 subset(atuswho.2013, select=common.cols),
                 subset(atuswho.2014, select=common.cols))

who.rost <- merge(atusrost, atuswho)
own.child <- subset(who.rost, TEAGE < 18 & TUWHO_CODE == 22)
own.child.2 <- unique(own.child[c("TUCASEID", "TUACTIVITY_N")])

## Only want the respondent from the roster file
roster <- subset(atusrost, TULINENO == 1)
## Create a new data frame from the respondent file and the subset of the
## roster file
resp.rost <- merge(atusresp, roster)
## Select women who are married, or hav an unmarried partner present.
##resp.rost <- subset(resp.rost, TRSPPRES == 1 | TRSPPRES == 2)
resp.rost <- subset(resp.rost, TRSPPRES == 1 & TESEX == 2 & TEAGE < 48)

a <- resp.rost
###if (iyear < 4) a$TUFINLWGT <- a$TU06FWGT
m <- dim(a)
b <- merge(atusact, a)

keepers <- c("TUCASEID",
             "TUACTDUR24",
             "TUFINLWGT",
             "TRTCC_LN",
             "TUTIER1CODE",
             "TUTIER2CODE",
             "TUTIER3CODE")

b <- subset(b , select = keepers)

atus.weights <- subset(b, select = c("TUCASEID", "TUFINLWGT"))
atus.weights <- aggregate(atus.weights, list(id=atus.weights$TUCASEID), max)

### Market work

market.work <- subset(b, (TUTIER1CODE == 5) |
                      (TUTIER1CODE == 18 & TUTIER2CODE == 5))
market.work <- aggregate(market.work, list(id=market.work$TUCASEID), sum)

colnames(market.work)[colnames(market.work) == "TUACTDUR24"] = "WORK"
colnames(market.work)[colnames(market.work) == "id"] = "TUCASEID"
market.work <- subset(market.work, select = c("TUCASEID", "WORK"))

### Primary childcare time

primary.childcare <- subset(b, TUTIER1CODE == 3 &
                            (TUTIER2CODE == 1 | TUTIER2CODE == 2 |
                             TUTIER2CODE == 3))
primary.childcare <- aggregate(primary.childcare, list(id=primary.childcare$TUCASEID), sum)
     
colnames(primary.childcare)[colnames(primary.childcare) == "TUACTDUR24"] = "PRIMARY"
colnames(primary.childcare)[colnames(primary.childcare) == "id"] = "TUCASEID"
primary.childcare <- subset(primary.childcare, select = c("TUCASEID", "PRIMARY"))

### Secondary childcare time

secondary.childcare <- subset(b, TRTCC_LN > 0 & (TUTIER1CODE == 12 |
                                                 TUTIER1CODE == 2 |
                                                 (TUTIER1CODE == 7 & TUTIER2CODE == 1 &
                                                  (TUTIER3CODE == 1 | TUTIER3CODE == 2 |
                                                   TUTIER3CODE == 3)) |
                                                 (TUTIER1CODE == 18 & TUTIER2CODE == 7) |
                                                 (TUTIER1CODE == 16 & TUTIER2CODE == 1 &
                                                  TUTIER3CODE == 4)))

secondary.childcare <- aggregate(secondary.childcare, list(id=secondary.childcare$TUCASEID), sum)
colnames(secondary.childcare)[colnames(secondary.childcare) == "TUACTDUR24"] <- "SECONDARY"

colnames(secondary.childcare)[colnames(secondary.childcare) == "TUACTDUR24"] = "SECONDARY"
colnames(secondary.childcare)[colnames(secondary.childcare) == "id"] = "TUCASEID"
secondary.childcare <- subset(secondary.childcare, select = c("TUCASEID", "SECONDARY"))

num.child <- unique(own.child[c("TUCASEID", "TEAGE")])
num.child$CHILD6 <- num.child$TEAGE < 6
num.child$CHILD12 <- num.child$TEAGE >= 6 & num.child$TEAGE < 13
num.child <- aggregate(num.child, list(id=num.child$TUCASEID),sum)

colnames(num.child)[colnames(num.child) == "id"] = "TUCASEID"
num.child <- subset(num.child, select = c("TUCASEID", "CHILD6", "CHILD12"))

childcare.time <- merge(primary.childcare, secondary.childcare, all=TRUE)

atus.time <- merge(childcare.time, market.work, all=TRUE)
atus.time[is.na(atus.time)] <- 0
atus.time <- aggregate(atus.time, list(id=atus.time$TUCASEID), max)
atus.time <- merge(atus.time, atus.weights)

### Drop observations that don't have both childcare time and children 12
### or younger. I.e., don't include "all=TRUE".
atus.time <- merge(atus.time, num.child)
atus.time <- subset(atus.time, CHILD6 < 3 & CHILD12 < 3 & !WORK>0 & CHILD6+CHILD12>0 & PRIMARY > 0 & SECONDARY > 0)

## Create dummy variables for number and ages of children

atus.time$c10 <- (atus.time$CHILD6 == 1) * (atus.time$CHILD12 == 0)
atus.time$c20 <- (atus.time$CHILD6 == 2) * (atus.time$CHILD12 == 0)
atus.time$c01 <- (atus.time$CHILD6 == 0) * (atus.time$CHILD12 == 1)
atus.time$c02 <- (atus.time$CHILD6 == 0) * (atus.time$CHILD12 == 2)
atus.time$c11 <- (atus.time$CHILD6 == 1) * (atus.time$CHILD12 == 1)
atus.time$c21 <- (atus.time$CHILD6 == 2) * (atus.time$CHILD12 == 1)
atus.time$c12 <- (atus.time$CHILD6 == 1) * (atus.time$CHILD12 == 2)
atus.time$c22 <- (atus.time$CHILD6 == 2) * (atus.time$CHILD12 == 2)

## Primary childcare time

atus.time$np <- atus.time$PRIMARY

## Secondary childcare time (housework and leisure)

atus.time$ns <- atus.time$SECONDARY
atus.time$wt <- atus.time$TUFINLWGT /sum(atus.time$TUFINLWGT)

## Dependent variable: a vector of zeros

atus.time$y <- array(0, length(atus.time$np))

## Weighted regression (using ATUS weights)

atus_w <- nls(y ~ -p01*c01 -p02*c02 -p10*c10 -p11*c11 -p12*c12 -p20*c20 -p21*c21 -p22*c22 
              + (nu*np^varphi +(1-nu)*ns^varphi)^(1/varphi),
              data=atus.time,
              start=list(p01=200, p02=200, p10=200, p11=200, p12=200, p20=200, p21=200, p22=200, 
                  nu=.5, varphi=.7),
              weights=wt)
     
atus_w_unres <- nls(y ~ c01*((nu01*np^varphi01 +(1-nu01)*ns^varphi01)^(1/varphi01) - p01)
                    + c02*((nu02*np^varphi02 +(1-nu02)*ns^varphi02)^(1/varphi02) - p02)
                    + c10*((nu10*np^varphi10 +(1-nu10)*ns^varphi10)^(1/varphi10) - p10)
                    + c11*((nu11*np^varphi11 +(1-nu11)*ns^varphi11)^(1/varphi11) - p11)
                    + c12*((nu12*np^varphi12 +(1-nu12)*ns^varphi12)^(1/varphi12) - p12)
                    + c20*((nu20*np^varphi20 +(1-nu20)*ns^varphi20)^(1/varphi20) - p20)
                    + c21*((nu21*np^varphi21 +(1-nu21)*ns^varphi21)^(1/varphi21) - p21)
                    + c22*((nu22*np^varphi22 +(1-nu22)*ns^varphi22)^(1/varphi22) - p22),
                    data=atus.time,
                    start=list(p01=200, p02=200, p10=200, p11=200, p12=200, p20=200, p21=200, p22=200, 
                        nu01=.5, nu02=.5, nu10=.5, nu11=.5, nu12=.5, nu20=.5, nu21=.5, nu22=.5, varphi01=.7, varphi02=.7, varphi10=.7, varphi11=.7, varphi12=.7, varphi20=.7, varphi21=.7, varphi22=.7),
                    weights=wt)

## Need to run the following
##print(atus_nw)
print(atus_w)
print(atus_w_unres)

summary(atus_w)
summary(atus_w_unres)


lrtest(atus_w, atus_w_unres)

sink()
