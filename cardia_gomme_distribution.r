### For "Market Work, Housework and Childcare: A Time Use
### Approach" by Emanuela Cardia and Paul Gomme, published in Review of
### Economic Dynamics.
###
### To run: source("cardia_gomme_distribution.r")
###
### Required American Time Use Survey data, available from 
### https://www.bls.gov/tus/
###
### Place data in subdirectories for each year (2003, 2004, etc.)
### For each age group (18-23, 24-29, 30-35, 36-41), report fraction of women
### who have 0, 1 or 2 children under the age of 6.
###
### This file produces weights used in the Fortran code.
###
### This file created by Paul Gomme is licensed under a Creative Commons
### Attribution 4.0 International License:
### https://creativecommons.org/licenses/by/4.0/

rm(list=ls())
options(digits=20)

age <- cbind(c(18,24,30,36,42,48,54,60,66,72,18),
             c(23,29,35,41,47,53,59,65,71,78,78))

NTUS <-  13

activity.file <- c("2003/atusact_2003.dat", "2004/atusact_2004.dat",
                   "2005/atusact_2005.dat", "2006/atusact_2006.dat",
                   "2007/atusact_2007.dat", "2008/atusact_2008.dat",
                   "2009/atusact_2009.dat", "2010/atusact_2010.dat",
                   "2011/atusact_2011.dat", "2012/atusact_2012.dat",
                   "2013/atusact_2013.dat", "2014/atusact_2014.dat",
                   "2015/atusact_2015.dat")
respondent.file <- c("2003/atusresp_2003.dat", "2004/atusresp_2004.dat",
                     "2005/atusresp_2005.dat", "2006/atusresp_2006.dat",
                     "2007/atusresp_2007.dat", "2008/atusresp_2008.dat",
                     "2009/atusresp_2009.dat", "2010/atusresp_2010.dat",
                     "2011/atusresp_2011.dat", "2012/atusresp_2012.dat",
                     "2013/atusresp_2013.dat", "2014/atusresp_2014.dat",
                     "2015/atusresp_2015.dat")
roster.file <- c("2003/atusrost_2003.dat", "2004/atusrost_2004.dat",
                 "2005/atusrost_2005.dat", "2006/atusrost_2006.dat",
                 "2007/atusrost_2007.dat", "2008/atusrost_2008.dat",
                 "2009/atusrost_2009.dat", "2010/atusrost_2010.dat",
                 "2011/atusrost_2011.dat", "2012/atusrost_2012.dat",
                 "2013/atusrost_2013.dat", "2014/atusrost_2014.dat",
                 "2015/atusrost_2015.dat")
who.file <- c("2003/atuswho_2003.dat", "2004/atuswho_2004.dat",
              "2005/atuswho_2005.dat", "2006/atuswho_2006.dat",
              "2007/atuswho_2007.dat", "2008/atuswho_2008.dat",
              "2009/atuswho_2009.dat", "2010/atuswho_2010.dat",
              "2011/atuswho_2011.dat", "2012/atuswho_2012.dat",
              "2013/atuswho_2013.dat", "2014/atuswho_2014.dat",
              "2015/atuswho_2015.dat")

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
atusact.2015 <- read.csv(activity.file[13])

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
common.cols <- intersect(common.cols, colnames(atusact.2015))

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
                 subset(atusact.2014, select=common.cols),
                 subset(atusact.2015, select=common.cols))

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
atusresp.2015 <- read.csv(respondent.file[13])

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
common.cols <- intersect(common.cols, colnames(atusresp.2015))

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
                 subset(atusresp.2014, select=common.cols),
                 subset(atusresp.2015, select=common.cols))

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
atusrost.2015 <- read.csv(roster.file[13])

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
common.cols <- intersect(common.cols, colnames(atusrost.2015))

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
                 subset(atusrost.2014, select=common.cols),
                 subset(atusrost.2015, select=common.cols))

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
atuswho.2015 <- read.csv(who.file[13])

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
common.cols <- intersect(common.cols, colnames(atuswho.2015))

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
                 subset(atuswho.2014, select=common.cols),
                 subset(atuswho.2015, select=common.cols))

## Only want the respondent from the roster file
roster <- subset(atusrost, TULINENO == 1)
## Create a new data frame from the respondent file and the subset of the
## roster file
resp.rost <- merge(atusresp, roster)
## keep only those columns that are used
resp.rost <- subset(resp.rost, select=c("TUCASEID", "TEAGE", "TRSPPRES", "TESEX", "TUFINLWGT"))

## create new data frame from roster and who files
who.rost <- merge(atusrost, atuswho)
## keep the own household children
own.children <- subset(who.rost, TEAGE < 18 & TUWHO_CODE == 22)
## keep only one observation per child
own.children <- unique(own.children[c('TUCASEID', 'TULINENO', 'TEAGE')])
## create variables for age bins on children
own.children$CHILD6 <- own.children$TEAGE < 6
own.children$CHILD12 <- own.children$TEAGE >= 6 & own.children$TEAGE < 12
own.children$CHILD18 <- own.children$TEAGE >= 12 & own.children$TEAGE < 18
## add up to get number of children in each bin
own.children <- aggregate(own.children, list(id=own.children$TUCASEID), sum)
## drop TUCASEID since it gets munged by the sum operation
own.children <- subset(own.children, select=c("id", "CHILD6", "CHILD12", "CHILD18"))
names(own.children)[1] <- "TUCASEID"

resp.children <- merge(resp.rost, own.children, all=TRUE)
## keep the women with a "spouse" present
resp.children <- subset(resp.children, TRSPPRES==1 & TESEX==2)
resp.children[is.na(resp.children)] <- 0

### Elements of w organized as:
### age bin of the woman
### number of children < 6 years old
### number of children 6-11 years old
### number of children 12-17 years old

w <- array(0, dim=c(4,3,3,3))

### 18-23

aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12== 0  & CHILD18== 0 )
w[1, 1 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12== 0  & CHILD18== 0 )
w[1, 2 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 0 )
w[1, 3 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12== 1  & CHILD18== 0 )
w[1, 1 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12== 1  & CHILD18== 0 )
w[1, 2 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 0 )
w[1, 3 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 0 )
w[1, 1 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 0 )
w[1, 2 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 0 )
w[1, 3 , 3 , 1 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12== 0  & CHILD18== 1 )
w[1, 1 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12== 0  & CHILD18== 1 )
w[1, 2 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 1 )
w[1, 3 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12== 1  & CHILD18== 1 )
w[1, 1 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12== 1  & CHILD18== 1 )
w[1, 2 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 1 )
w[1, 3 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 1 )
w[1, 1 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 1 )
w[1, 2 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 1 )
w[1, 3 , 3 , 2 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12== 0  & CHILD18>= 2 )
w[1, 1 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12== 0  & CHILD18>= 2 )
w[1, 2 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12== 0  & CHILD18>= 2 )
w[1, 3 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12== 1  & CHILD18>= 2 )
w[1, 1 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12== 1  & CHILD18>= 2 )
w[1, 2 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12== 1  & CHILD18>= 2 )
w[1, 3 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 0  & CHILD12>= 2  & CHILD18>= 2 )
w[1, 1 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6== 1  & CHILD12>= 2  & CHILD18>= 2 )
w[1, 2 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=18 & TEAGE<=23 & CHILD6>= 2  & CHILD12>= 2  & CHILD18>= 2 )
w[1, 3 , 3 , 3 ] = sum(aaa$TUFINLWGT)


### 24-29

aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12== 0  & CHILD18== 0 )
w[2, 1 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12== 0  & CHILD18== 0 )
w[2, 2 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 0 )
w[2, 3 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12== 1  & CHILD18== 0 )
w[2, 1 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12== 1  & CHILD18== 0 )
w[2, 2 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 0 )
w[2, 3 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 0 )
w[2, 1 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 0 )
w[2, 2 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 0 )
w[2, 3 , 3 , 1 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12== 0  & CHILD18== 1 )
w[2, 1 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12== 0  & CHILD18== 1 )
w[2, 2 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 1 )
w[2, 3 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12== 1  & CHILD18== 1 )
w[2, 1 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12== 1  & CHILD18== 1 )
w[2, 2 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 1 )
w[2, 3 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 1 )
w[2, 1 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 1 )
w[2, 2 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 1 )
w[2, 3 , 3 , 2 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12== 0  & CHILD18>= 2 )
w[2, 1 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12== 0  & CHILD18>= 2 )
w[2, 2 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12== 0  & CHILD18>= 2 )
w[2, 3 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12== 1  & CHILD18>= 2 )
w[2, 1 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12== 1  & CHILD18>= 2 )
w[2, 2 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12== 1  & CHILD18>= 2 )
w[2, 3 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 0  & CHILD12>= 2  & CHILD18>= 2 )
w[2, 1 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6== 1  & CHILD12>= 2  & CHILD18>= 2 )
w[2, 2 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=24 & TEAGE<=29 & CHILD6>= 2  & CHILD12>= 2  & CHILD18>= 2 )
w[2, 3 , 3 , 3 ] = sum(aaa$TUFINLWGT)

### 30-35

aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12== 0  & CHILD18== 0 )
w[3, 1 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12== 0  & CHILD18== 0 )
w[3, 2 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 0 )
w[3, 3 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12== 1  & CHILD18== 0 )
w[3, 1 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12== 1  & CHILD18== 0 )
w[3, 2 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 0 )
w[3, 3 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 0 )
w[3, 1 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 0 )
w[3, 2 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 0 )
w[3, 3 , 3 , 1 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12== 0  & CHILD18== 1 )
w[3, 1 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12== 0  & CHILD18== 1 )
w[3, 2 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 1 )
w[3, 3 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12== 1  & CHILD18== 1 )
w[3, 1 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12== 1  & CHILD18== 1 )
w[3, 2 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 1 )
w[3, 3 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 1 )
w[3, 1 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 1 )
w[3, 2 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 1 )
w[3, 3 , 3 , 2 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12== 0  & CHILD18>= 2 )
w[3, 1 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12== 0  & CHILD18>= 2 )
w[3, 2 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12== 0  & CHILD18>= 2 )
w[3, 3 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12== 1  & CHILD18>= 2 )
w[3, 1 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12== 1  & CHILD18>= 2 )
w[3, 2 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12== 1  & CHILD18>= 2 )
w[3, 3 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 0  & CHILD12>= 2  & CHILD18>= 2 )
w[3, 1 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6== 1  & CHILD12>= 2  & CHILD18>= 2 )
w[3, 2 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=30 & TEAGE<=35 & CHILD6>= 2  & CHILD12>= 2  & CHILD18>= 2 )
w[3, 3 , 3 , 3 ] = sum(aaa$TUFINLWGT)

### 36-41

aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12== 0  & CHILD18== 0 )
w[4, 1 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12== 0  & CHILD18== 0 )
w[4, 2 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 0 )
w[4, 3 , 1 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12== 1  & CHILD18== 0 )
w[4, 1 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12== 1  & CHILD18== 0 )
w[4, 2 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 0 )
w[4, 3 , 2 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 0 )
w[4, 1 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 0 )
w[4, 2 , 3 , 1 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 0 )
w[4, 3 , 3 , 1 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12== 0  & CHILD18== 1 )
w[4, 1 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12== 0  & CHILD18== 1 )
w[4, 2 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12== 0  & CHILD18== 1 )
w[4, 3 , 1 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12== 1  & CHILD18== 1 )
w[4, 1 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12== 1  & CHILD18== 1 )
w[4, 2 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12== 1  & CHILD18== 1 )
w[4, 3 , 2 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12>= 2  & CHILD18== 1 )
w[4, 1 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12>= 2  & CHILD18== 1 )
w[4, 2 , 3 , 2 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12>= 2  & CHILD18== 1 )
w[4, 3 , 3 , 2 ] = sum(aaa$TUFINLWGT)
      
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12== 0  & CHILD18>= 2 )
w[4, 1 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12== 0  & CHILD18>= 2 )
w[4, 2 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12== 0  & CHILD18>= 2 )
w[4, 3 , 1 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12== 1  & CHILD18>= 2 )
w[4, 1 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12== 1  & CHILD18>= 2 )
w[4, 2 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12== 1  & CHILD18>= 2 )
w[4, 3 , 2 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 0  & CHILD12>= 2  & CHILD18>= 2 )
w[4, 1 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6== 1  & CHILD12>= 2  & CHILD18>= 2 )
w[4, 2 , 3 , 3 ] = sum(aaa$TUFINLWGT)
aaa = subset(resp.children, TEAGE>=36 & TEAGE<=41 & CHILD6>= 2  & CHILD12>= 2  & CHILD18>= 2 )
w[4, 3 , 3 , 3 ] = sum(aaa$TUFINLWGT)

## Continuation probability (for 36-41 year old women)
cp <- array(0, dim=c(3,3,3))
cp[1,,] <- w[4,1,,]
cp[2,,] <- w[4,2,,]
cp[3,,] <- w[4,3,,]

## Probability that a woman aged 30-35 has a particular birth history
p <-  w[3,,,] / sum(w[3,,,])

##h_weight <- vector(mode='numeric', length=81)

### The array f is the probability of a particular birth history, ordered as:
### number of children born when 18-23
### number of children born when 24-29
### number of children born when 30-35
### number of children born when 36-41

sink("cardia_gomme_distribution.output", append=FALSE, split=FALSE)

f <- array(0, dim=c(3,3,3,3))
for (i in 1:3)
  {
    for (j in 1:3)
      {
        for (k in 1:3)
          {
            for (l in 1:3)
              {
                f[l,k,j,i] <- cp[i,j,k]*p[j,k,l] / sum(cp[,j,k])
                cat(sprintf('f(%i,%i,%i,%i)=%.12f\n', l,k,j,i,f[l,k,j,i]))
              }
          }
      }
  }

w.18.23<- vector(mode='numeric', length=3)

for (l in 1:3)
  {
    xxx <- sum(w[1,l,,]) / sum(w[1,,,])
    w.18.23[l] <- xxx
    cat(sprintf('w_18_23(%i)=%.12f\n', l, xxx))
  }

w.24.29<- vector(mode='numeric', length=3)

for (l in 1:3)
  {
    xxx <- sum(w[2,l,,]) / sum(w[2,,,])
    w.24.29[l] <- xxx
    cat(sprintf('w_24_29(%i)=%.12f\n', l, xxx))
  }

w.30.35<- vector(mode='numeric', length=3)

for (l in 1:3)
  {
    xxx <- sum(w[3,l,,]) / sum(w[3,,,])
    w.30.35[l] <- xxx
    cat(sprintf('w_30_35(%i)=%.12f\n', l, xxx))
  }

w.36.41<- vector(mode='numeric', length=3)

for (l in 1:3)
  {
    xxx <- sum(w[4,l,,]) / sum(w[4,,,])
    w.36.41[l] <- xxx
    cat(sprintf('w_36_41(%i)=%.12f\n', l, xxx))
  }
        
sink()


