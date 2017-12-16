### For "Market Work, Housework and Childcare: A Time Use
### Approach" by Emanuela Cardia and Paul Gomme, published in Review of
### Economic Dynamics.
###
### To run: source("cardia_gomme_full_atus.r")
###
### Required American Time Use Survey data, available from 
### https://www.bls.gov/tus/
### Place data in subdirectories for each year (2003, 2004, etc.)
###
### This file produces summary statistics in Table 1 in the paper.
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
                     "2015/atusact_2015.dat")
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
own.kids <- subset(who.rost, TEAGE < 18 & TUWHO_CODE == 22)
own.kids.2 <- unique(own.kids[c("TUCASEID", "TUACTIVITY_N")])

## Only want the respondent from the roster file
roster <- subset(atusrost, TULINENO == 1)
## Create a new data frame from the respondent file and the subset of the
## roster file
resp.rost <- merge(atusresp, roster)
## Select women who are married, or hav an unmarried partner present.
##resp.rost <- subset(resp.rost, TRSPPRES == 1 | TRSPPRES == 2)
resp.rost <- subset(resp.rost, TRSPPRES == 1 & TESEX == 2)

tus <- data.frame(matrix(0, nrow=11, ncol=11))
names(tus) <- c("Observations", "Personal Care", "Leisure",
                "Market Work", "Housework", "Housework (broad)",
                "Primary Childcare", "Secondary Childcare",
                "Secondary Childcare (with housework)",
                "Secondary Childcare (with broad housework)",
                "Secondary Childcare (with leisure)")

for (i in 1:11) {
    a <- subset(resp.rost, (TEAGE >= age[i,1] & TEAGE <= age[i,2]))
    m <- dim(a)
    b <- merge(atusact, a)
    b <- data.frame(TUCASEID=b$TUCASEID, TUACTDUR24=b$TUACTDUR24,
                    TRTCC_LN=b$TRTCC_LN,
                    TUTIER1CODE=b$TUTIER1CODE, TUTIER2CODE=b$TUTIER2CODE,
                    TUTIER3CODE=b$TUTIER3CODE)

    personal.care <- subset(b, TUTIER1CODE == 1)
    ##leisure <- subset(b, TUTIER1CODE == 12)
    leisure <- subset(b, TUTIER1CODE == 12 | TUTIER1CODE == 13)
    market.work <- subset(b, (TUTIER1CODE == 5) |
                          (TUTIER1CODE == 18 & TUTIER2CODE == 5))
    housework <- subset(b, TUTIER1CODE == 2)
    total.housework <- subset(b, TUTIER1CODE == 2 |
                              (TUTIER1CODE == 7 & TUTIER2CODE == 1 &
                               (TUTIER3CODE == 1 | TUTIER3CODE == 2 |
                                TUTIER3CODE == 3)) |
                              (TUTIER1CODE == 18 & TUTIER2CODE == 7) |
                              (TUTIER1CODE == 16 & TUTIER2CODE == 1 &
                               TUTIER3CODE == 4))
    primary.childcare <- subset(b, TUTIER1CODE == 3 &
                                (TUTIER2CODE == 1 | TUTIER2CODE == 2 |
                                 TUTIER2CODE == 3))

    secondary.childcare.housework <- subset(housework, TRTCC_LN > 0)
    secondary.childcare.total.housework <- subset(total.housework, TRTCC_LN > 0)
    secondary.childcare.leisure <- subset(leisure, TRTCC_LN > 0)
    
    personal.care <- aggregate(personal.care,
                               list(id=personal.care$TUCASEID), sum)
    leisure <- aggregate(leisure, list(id=leisure$TUCASEID), sum)
    market.work <- aggregate(market.work, list(id=market.work$TUCASEID), sum)
    housework <- aggregate(housework, list(id=housework$TUCASEID), sum)
    total.housework <- aggregate(total.housework,
                                 list(id=total.housework$TUCASEID), sum)

    secondary.childcare <- subset(b, TRTCC_LN > 0 & !(TUTIER1CODE == 3 &
                                (TUTIER2CODE == 1 | TUTIER2CODE == 2 |
                                 TUTIER2CODE == 3)))

    if (dim(secondary.childcare)[1] > 0)
        secondary.childcare <- aggregate(secondary.childcare,
                                   list(id=secondary.childcare$TUCASEID), sum)

    if (dim(secondary.childcare.housework)[1] > 0)
        secondary.childcare.housework <- aggregate(secondary.childcare.housework,
                                   list(id=secondary.childcare.housework$TUCASEID), sum)

    if (dim(secondary.childcare.total.housework)[1] > 0)
        secondary.childcare.total.housework <- aggregate(secondary.childcare.total.housework,
                                   list(id=secondary.childcare.total.housework$TUCASEID), sum)

    if (dim(secondary.childcare.leisure)[1] > 0)
        secondary.childcare.leisure <- aggregate(secondary.childcare.leisure,
                                   list(id=secondary.childcare.leisure$TUCASEID), sum)

    if (dim(primary.childcare)[1] > 0)
        primary.childcare <- aggregate(primary.childcare,
                                    list(id=primary.childcare$TUCASEID), sum)
    
    tus[i,1] <- m[1]
    w <- subset(a, TUCASEID %in% personal.care$id)
    tus[i,2] <- sum(personal.care$TUACTDUR24*w$TUFINLWGT)/sum(a$TUFINLWGT)
    w <- subset(a, TUCASEID %in% leisure$id)
    tus[i,3] <- sum(leisure$TUACTDUR24*w$TUFINLWGT)/sum(a$TUFINLWGT)
    w <- subset(a, TUCASEID %in% market.work$id)
    tus[i,4] <- sum(market.work$TUACTDUR24*w$TUFINLWGT)/sum(a$TUFINLWGT)
    w <- subset(a, TUCASEID %in% housework$id)
    tus[i,5] <- sum(housework$TUACTDUR24*w$TUFINLWGT)/sum(a$TUFINLWGT)
    w <- subset(a, TUCASEID %in% total.housework$id)
    tus[i,6] <- sum(total.housework$TUACTDUR24*w$TUFINLWGT)/sum(a$TUFINLWGT)

    if (dim(primary.childcare)[1] > 0) {
        w <- subset(a, TUCASEID %in% primary.childcare$id)
        tus[i,7] <- sum(primary.childcare$TUACTDUR24*w$TUFINLWGT)/sum(a$TUFINLWGT)
    }
    if (dim(secondary.childcare)[1] > 0) {
        w <- subset(a, TUCASEID %in% secondary.childcare$id)
        tus[i,8] <- sum(secondary.childcare$TRTCC_LN*w$TUFINLWGT)/sum(a$TUFINLWGT)
    }
    if (dim(secondary.childcare.housework)[1] > 0) {
        w <- subset(a, TUCASEID %in% secondary.childcare.housework$id)
        tus[i,9] <- sum(secondary.childcare.housework$TRTCC_LN*w$TUFINLWGT)/sum(a$TUFINLWGT)
    }
    if (dim(secondary.childcare.total.housework)[1] > 0) {
        w <- subset(a, TUCASEID %in% secondary.childcare.total.housework$id)
        tus[i,10] <- sum(secondary.childcare.total.housework$TRTCC_LN*w$TUFINLWGT)/sum(a$TUFINLWGT)
    }
    if (dim(secondary.childcare.leisure)[1] > 0) {
        w <- subset(a, TUCASEID %in% secondary.childcare.leisure$id)
        tus[i,11] <- sum(secondary.childcare.leisure$TRTCC_LN*w$TUFINLWGT)/sum(a$TUFINLWGT)
    }
}
write.table(format(tus,digits=5), file='cardia_gomme_full_atus.csv', sep=',')

