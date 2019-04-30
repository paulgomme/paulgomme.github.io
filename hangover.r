sink("hangover.output", append=FALSE, split=FALSE)

###source('usdata.r') ### Obtain from http://paulgomme.github.io
#source('../../../../Research/usdata/usdata.r') ### Obtain from http://paulgomme.github.io

### May need to install a number of packages:
###
### install.packages('quantmod')
### install.packages('Hmisc')
### install.packages('mFilter')
### install.packages('FRAPO')

### HP filter smoothing parameters. lambda.high is the very smooth line to
### take out the long run trend; value for quarterly data. lambda.low is to
### get a trend line that smooths out high frequency fluctuations. lambda.q2m
### is the conversion factor for applying the previous values to monthly
### rather than quarterly data (based on recommendations in Ravn and Uhlig).
lambda.high <- 100000
lambda.low <- 1
lambda.q2m <- 3^4

### Interpolate quarterly data to monthly frequency using a smooth spline
quarter2month.spline.smooth <- function(data.in)
  {
    dt.start <- start(data.in)
    dt.end <- end(data.in)
    yr.start <- as.numeric(format(dt.start, "%Y"))
    yr.end <- as.numeric(format(dt.end, "%Y"))
    mon.start <- as.numeric(format(dt.start, "%m"))
    mon.end <- as.numeric(format(dt.end, "%m"))

    yr.q <- seq(yr.start+(mon.start-1)/12, yr.end+(mon.end-1)/12, 1/4)
    yr.m <- seq(yr.start+(mon.start-1)/12, yr.end+(mon.end-1)/12, 1/12)
    t1 <- as.Date(sprintf('%i-%i-01', yr.start, mon.start))
    t2 <- as.Date(sprintf('%i-%i-01', yr.end, mon.end))
    
    work.spl <- smooth.spline(yr.q, data.in)
    work.sp <- predict(work.spl, yr.m)
    data.out <- xts(work.sp$y, order.by=seq(dt.start, dt.end, by='month'))
    data.out
  }

### Interpolate quarterly data to monthly frequency using a spline (no smoothing)
quarter2month.spline.interp <- function(data.in)
  {
    dt.start <- start(data.in)
    dt.end <- end(data.in)
    yr.start <- as.numeric(format(dt.start, "%Y"))
    yr.end <- as.numeric(format(dt.end, "%Y"))
    mon.start <- as.numeric(format(dt.start, "%m"))
    mon.end <- as.numeric(format(dt.end, "%m"))

    yr.q <- seq(yr.start+(mon.start-1)/12, yr.end+(mon.end-1)/12, 1/4)

    work.sp <- spline(yr.q, data.in)

    dt.end <- as.Date(sprintf('%i-%i-01', yr.end, mon.end+2))
    
    data.out <- xts(work.sp$y, order.by=seq(dt.start, dt.end, by='month'))
    data.out
  }

### Interpolate annual data to quarterly frequency using a spline (no smoothing)
annual2quarter.spline.interp <- function(data.in)
  {
    dt.start <- start(data.in)
    dt.end <- end(data.in)
    yr.start <- as.numeric(format(dt.start, "%Y"))
    yr.end <- as.numeric(format(dt.end, "%Y"))
    mon.start <- as.numeric(format(dt.start, "%m"))
    mon.end <- as.numeric(format(dt.end, "%m"))

    yr.q <- seq(yr.start+(mon.start-1)/12, yr.end+(mon.end-1)/12, 1)

    work.sp <- spline(yr.q, data.in, n=4*length(data.in))

    dt.end <- as.Date(sprintf('%i-%i-01', yr.end, mon.end+9))

    data.out <- xts(work.sp$y, order.by=seq(dt.start, dt.end, by='quarter'))
    data.out
  }

### Use a spline to smooth monthly data
month.spline.smooth <- function(data.in)
  {
    dt.start <- start(data.in)
    dt.end <- end(data.in)
    yr.start <- as.numeric(format(dt.start, "%Y"))
    yr.end <- as.numeric(format(dt.end, "%Y"))
    mon.start <- as.numeric(format(dt.start, "%m"))
    mon.end <- as.numeric(format(dt.end, "%m"))

    yr.m <- seq(yr.start+(mon.start-1)/12, yr.end+(mon.end-1)/12, 1/12)

    work.spl <- smooth.spline(yr.m, data.in)
    work.sp <- predict(work.spl, yr.m)
    data.out <- xts(work.sp$y, order.by=seq(dt.start, dt.end, by='month'))
    data.out
  }

quarter_return_business_capital_after_tax = 
  (quarter_business_income_after_tax/4 / 
        (quarter_deflator_consumption/100) / 
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke)) + 1)

N <- length(quarter_return_business_capital_after_tax)

month_return_business_capital_after_tax <- quarter2month.spline.interp(quarter_return_business_capital_after_tax[2:N])

fredsym <- c(
    ## Monthly data
    "FII5",	# 5-Year Treasury Inflation-Indexed Security, Constant Maturity
    "FII10",	# 10-Year Treasury Inflation-Indexed Security, Constant Maturity
    "FII20",	# 20-Year Treasury Inflation-Indexed Security, Constant Maturity
    "FII30"	# 20-Year Treasury Inflation-Indexed Security, Constant Maturity
    )

for (i in 1:length(fredsym))
  {
    getSymbols(fredsym[i],src="FRED")
  }

## Convert to a monthly return
Rk <- month_return_business_capital_after_tax^(1/3)

Rb <- (1+FII20/100)^(1/12)

## The financial shock
Omega <- Rb / Rk

Omega.smooth <- month.spline.smooth(Omega)

equity_premium <- month_return_business_capital_after_tax^4 / (1+FII20/100)

quarter_tau_c_spline <- annual2quarter.spline.interp((annual_tau_c))

quarter_tau_c_new <- rbind(quarter_tau_c_spline[paste('1947-01-01','1958-10-01',sep='/')],
                           quarter_tau_c)

smooth.month.tau.c <- quarter2month.spline.smooth(quarter_tau_c_new)
smooth.month.tau.n <- quarter2month.spline.smooth(quarter_tau_n)
smooth.month.tau.k <- quarter2month.spline.smooth(quarter_tau_k)

options(digits=20)

print("Mean of Omega, 2005-01 to 2007-12")
print(mean(Omega[paste('2005-01-01','2007-12-01',sep='/')]))
print("Mean of Omega, 2005-01 to 2007-12, annualized")
print(mean(Omega[paste('2005-01-01','2007-12-01',sep='/')])^12)

print("Mean of equity premium, 2005-01 to 2007-12")
print(mean(equity_premium[paste('2005-01-01','2007-12-01',sep='/')]))
print("Mean of equity premium, 2005-01 to 2007-12, annualized")
print(mean(equity_premium[paste('2005-01-01','2007-12-01',sep='/')])^12)

print("Mean of tau_c, 2005-01 to 2007-12")
print(mean(quarter_tau_c[paste('2005-01-01','2007-12-01',sep='/')]))
print("Mean of tau_n, 2005-01 to 2007-12")
print(mean(quarter_tau_n[paste('2005-01-01','2007-12-01',sep='/')]))
print("Mean of tau_k, 2005-01 to 2007-12")
print(mean(quarter_tau_k[paste('2005-01-01','2007-12-01',sep='/')]))

print("Mean of Omega, 2000-12 to 2007-12")
print(mean(Omega[paste('2000-12-01','2007-12-01',sep='/')]))
print("Mean of Omega, 2000-12 to 2007-12, annualized")
print(mean(Omega[paste('2000-12-01','2007-12-01',sep='/')])^12)

print("Mean of equity_premium, 2000-12 to 2007-12")
print(mean(equity_premium[paste('2000-12-01','2007-12-01',sep='/')]))
print("Mean of equity_premium, 2000-12 to 2007-12, annualized")
print(mean(equity_premium[paste('2000-12-01','2007-12-01',sep='/')])^12)

print("Mean of tau_c, 2000-12 to 2007-12")
print(mean(quarter_tau_c[paste('2000-12-01','2007-12-01',sep='/')]))
print("Mean of tau_n, 2000-12 to 2007-12")
print(mean(quarter_tau_n[paste('2000-12-01','2007-12-01',sep='/')]))
print("Mean of tau_k, 2000-12 to 2007-12")
print(mean(quarter_tau_k[paste('2000-12-01','2007-12-01',sep='/')]))

### fred-u.r

fredsym <- c("UNRATE", # Civilian Unemployment Rate
             "UNEMPLOY", # Unemployment Level
             "UEMPLT5", # Number of Civilians Unemployed for Less Than 5 Weeks
             "CE16OV", # Civilian Employment
             "JTSJOL", # Job Openings: Total Nonfarm
             "JTSTSL", # Total separations: total nonfarm
             "JTSQUR", # Quits: Total Nonfarm
             "JTSHIL", # Hires: Total Nonfarm
             "CNP16OV", # Civilian Noninstitutional Population
             "W825RC1", # Personal current transfer receipts: Government social benefits to persons: Unemployment insurance
             "A063RC1", # Personal current transfer receipts: Government social benefits to persons
             "CPIAUCSL" # Consumer price index for all urban consumers: all items
             )

for   (i in 1:length(fredsym)){
getSymbols(fredsym[i],src="FRED")
}

N <- length(CE16OV)

UEMPLT5[553:N,] <- UEMPLT5[553:N,] * 1.1
F <- 1 - (UNEMPLOY[2:N,] - UEMPLT5[2:N,]) / UNEMPLOY[1:(N-1),]
cps.f <- -log(1-F)

u <- UNEMPLOY
e <- CE16OV

#joltsraw <- read.csv('jolts.csv')
hwi <- read.csv('hwi-index.csv')

#jolts.hires <- ts(joltsraw$JTS10000000HIL, start=c(2000,12), frequency=12)
#jolts.separations <- ts(joltsraw$JTS10000000TSL, start=c(2000,12), frequency=12)
#jolts.openings <- ts(joltsraw$JTS10000000JOL, start=c(2000,12), frequency=12)
jolts.hires <- ts(JTSHIL, start=c(2000,12), frequency=12)
jolts.separations <- ts(JTSTSL, start=c(2000,12), frequency=12)
jolts.openings <- ts(JTSJOL, start=c(2000,12), frequency=12)
jolts.openings <- 100*jolts.openings / jolts.openings[1]
v <- ts(hwi$hwi, start=c(1951,1), frequency=12)

emp <- ts(e, start=c(1948,1), frequency=12)
unemp <- ts(u, start=c(1948,1), frequency=12)
jolts.s <- jolts.separations / emp
jolts.f <- jolts.hires / unemp

Emp <- vector(mode="numeric", length=166)
Emp[1] <- e[636]
Unemp <- u[636:803]

for (i in 1:165)
  {
    Emp[i+1] <- (1-jolts.s[i])*Emp[i] + jolts.f[i]*Unemp[i]
  }
Emp <- ts(Emp, start=c(2000,12), frequency=12)

jolts.s <- as.xts(jolts.s)
jolts.f <- as.xts(jolts.f)
emp <- as.xts(emp)
unemp <- as.xts(unemp)
Emp <- as.xts(Emp)

finds <- function(cps.s)
  {
    y <- (1-exp(-cps.f-cps.s)) * (u[1:(N-1)] + e[1:(N-1)]) * cps.s / (cps.f+cps.s) + exp(-cps.f-cps.s) * u[1:(N-1)] - u[2:N]
    y
  }

cps.s <- cps.f
ans <- nleqslv(cps.s, finds, control=list(ftol=1e-12,xtol=1e-16))

f21 <- rep(1/7,7)

cps.s <- ts(ans$x, start=c(1948,1), frequency=12)
cps.f <- ts(cps.f, start=c(1948,2), frequency=12)

###s <- 1 - exp(-s)
###f <- 1 - exp(-f)
###
###s.filt <- filter(s, f21, sides=2)
###f.filt <- filter(f, f21, sides=2)
###
###s.hp <- hpfilter(s, type="lambda", freq=81)
###f.hp <- hpfilter(f, type="lambda", freq=81)
###e.ln <- log(emp)
###e.hp <- hpfilter(e.ln, type="lambda", freq=81)

jolts.s = ts(jolts.s, start=c(2000,12), frequency=12)
jolts.f = ts(jolts.f, start=c(2000,12), frequency=12)

jolts.s.hp <- hpfilter(jolts.s, type="lambda", freq=lambda.low*lambda.q2m)
jolts.f.hp <- hpfilter(jolts.f, type="lambda", freq=lambda.low*lambda.q2m)

v.hp <- hpfilter(v, type="lambda", freq=lambda.low*lambda.q2m)
jolts.openings.hp <- hpfilter(jolts.openings, type="lambda", freq=lambda.low*lambda.q2m)

unrate <- ts(UNRATE/100, start=c(1948,1), frequency=12)
# The window command selects a part of a time series, in this case, starting
# Dec. 2000
ucps <- window(unrate, start=c(2000,12))
ujolts <- ucps
scps <- window(cps.s, start=c(2000,12))
fcps <- window(cps.f, start=c(2000,12))
ecps = 1 - ucps
ejolts = 1 - ujolts
T <- dim(ucps)[1]
for (t in 1:(T-1))
  {
    ecps[t+1] <- (1-scps[t])*ecps[t] + fcps[t]*(1-ecps[t])
    ejolts[t+1] <- (1-jolts.s[t])*ejolts[t] + jolts.f[t]*(1-ejolts[t])
  }
ucps <- 1 - ecps
ujolts <- 1 - ejolts

dt.start <- start(jolts.s)
dt.end <- end(jolts.s)

yr <- seq(dt.start[1]+(dt.start[2]-1)/12, dt.end[1]+(dt.end[2]-1)/12, 1/12)
#jolts.s.spl <- smooth.spline(yr, jolts.s)
#jolts.s.sp <- predict(jolts.s.spl, yr)
#jolts.f.spl <- smooth.spline(yr, jolts.f)
#jolts.f.sp <- predict(jolts.f.spl, yr)

#jolts.s.smooth <- ts(jolts.s.sp$y, start=dt.start, frequency=12)
#jolts.f.smooth <- ts(jolts.f.sp$y, start=dt.start, frequency=12)

jolts.s.smooth <- hpfilter(jolts.s, type="lambda", freq=lambda.low*lambda.q2m)$trend
jolts.f.smooth <- hpfilter(jolts.f, type="lambda", freq=lambda.low*lambda.q2m)$trend

# Real UI benefit per unemployed
b <- W825RC1 / UNEMPLOY / CPIAUCSL

# Lump-sum transfer (real, per capita)
# 2019-04-06: Scaled the lump-sum transfer by 10000
tau.lump <- 10000 * (A063RC1 - W825RC1) / CNP16OV / CPIAUCSL

t1 <- start(jolts.s)
t1 <- as.Date(sprintf('%i-%i-1', t1[1], t1[2]))
t2 <- end(jolts.s)
t2 <- as.Date(sprintf('%i-%i-1', t2[1], t2[2]))
jolts.s <- xts(jolts.s, order.by=seq(t1, t2, by='month'))

jolts.f <- xts(jolts.f, order.by=seq(t1, t2, by='month'))
jolts.s.smooth <- xts(jolts.s.smooth, order.by=seq(t1, t2, by='month'))
jolts.f.smooth <- xts(jolts.f.smooth, order.by=seq(t1, t2, by='month'))

vu <- JTSJOL / UNEMPLOY

fmean <- mean(as.xts(cps.f)[paste('2005-01-01','2007-12-01',sep='/')])
smean <- mean(as.xts(cps.s)[paste('2005-01-01','2007-12-01',sep='/')])
umean <- mean(UNRATE[paste('2005-01-01','2007-12-01',sep='/')])
vumean <- mean(vu[paste('2005-01-01','2007-12-01',sep='/')])

print("")
print("CPS")
print("Mean job-separation prob., 2005m1-2007m12")
print(smean)
print("Mean job-finding prob., 2005m1-2007m12")
print(fmean)
print("Mean unemployment rate (%), 2005m1-2007m12")
print(umean)
print("v-u ratio, 2005m1-2007m12")
print(vumean)

fmean <- mean(as.xts(jolts.f)[paste('2000-12-01','2007-12-01',sep='/')])
smean <- mean(as.xts(jolts.s)[paste('2000-12-01','2007-12-01',sep='/')])
umean <- mean(UNRATE[paste('2000-12-01','2007-12-01',sep='/')])
vumean <- mean(vu[paste('2000-12-01','2007-12-01',sep='/')])

print("")
print("JOLTS")
print("Mean job-separation prob., 2000m12-2007m12")
print(smean)
print("Mean job-finding prob., 2000m12-2007m12")
print(fmean)
print("Mean unemployment rate (%), 2000m12-2007m12")
print(umean)
print("Mean v-u ratio, 2000m12-2007m12")
print(vumean)

library(x12)
###x12path("/usr/local/bin/x12a")

fredsym <- c("PCECC96", # Real personal consumption expenditures
             "HOANBS", # Nonfarm Business Sector: Hours of All Persons
             "GCEC96", # Real Government Consumption Expenditures & Gross Investment
             "GPDIC96", # Real Gross Private Domestic Investment, 3 decimal
             "GCE", # Government expenditures on consumption and investment
             "GDP", # Gross Domestic Product
             "GDPC96", # Real Gross Domestic Product, 3 Decimal
             "CNP16OV", # Civilian Noninstitutional Population
             "UNRATE", # Civilian Noninstitutional Population
             "GS10", # 10-Year Treasury Constant Maturity Rate
             "DPCERD3Q086SBEA", # Personal consumption expenditures (implicit price deflator)
             "USREC", # BER based Recession Indicators for the United States from the Period following the Peak through the Trough
             "GDPDEF", # Gross Domestic Product: Implicit Price Deflator
             "CPALTT01USQ661S", # Consumer Price Index: Total All Items for the United States
             "GFDEGDQ188S", #  Federal Debt: Total Public Debt as Percent of Gross Domestic Product
             "GFDGDPA188S", # Gross Federal Debt as Percent of Gross Domestic Product
             "M318501Q027NBEA", # Federal government budget surplus or deficit (-)
             "A063RC1", # Personal current transfer receipts: Government social benefits to persons
             "CPIAUCSL", # Consumer price index for all urban consumers: all items
             "DPCERD3Q086SBEA", # PCE deflator (2009 = 100)
             "LES1252881600Q", # Employed full time: Median usual weekly real earnings: Wage and salary workers: 16 years and over
             "UNEMPLOY") # Total unemployed (monthly)

for   (i in 1:length(fredsym))
{
    getSymbols(fredsym[i],src="FRED")
}

real.wage <- LES1252881600Q

pop <- aggregate(CNP16OV, as.yearqtr, mean) / 1000
unemploy <- aggregate(UNEMPLOY, as.yearqtr, mean)
###c <- aggregate(PCECC96, as.yearqtr, mean)
###h <- aggregate(HOANBS, as.yearqtr, mean)
###g <- aggregate(GCEC96, as.yearqtr, mean)
###x <- aggregate(GPDIC96, as.yearqtr, mean)
###y <- aggregate(GDPC96, as.yearqtr, mean)
####u <- aggregate(UNRATE, as.yearqtr, mean)
###unemploy <- aggregate(UNEMPLOY, as.yearqtr, mean)

c <- PCECC96
h <- HOANBS
g <- GCEC96
x <- GPDIC96
y <- GDPC96

surp <- ts(M318501Q027NBEA, start=c(1959,3), frequency=4)
surp <- surp+500
sx12 <- x12(new("x12Single", ts=surp))
surplus <- as.xts(sx12@x12Output@d11) - 500

c.pc <- log(c / as.xts(pop))
h.pc <- log(h / as.xts(pop))
g.pc <- log(g / as.xts(pop))
x.pc <- log(x / as.xts(pop))
y.pc <- log(y / as.xts(pop))
###debt.y <- ts(GFDEGDQ188S, start=c(1966,1), frequency=4)
debt.y <- GFDEGDQ188S
###surplus.y <- as.ts(surplus) / as.ts(y)
surplus.y <- surplus / y

p <- log(DPCERD3Q086SBEA)
r.pcedef <- GS10 - diff(p, lag=4)
p <- log(GDPDEF)
r.gdpdef <- GS10 - diff(p, lag=4)
p <- log(CPALTT01USQ661S)
r.cpi <- GS10 - diff(p, lag=4)

# Real UI benefit
b <- W825RC1 / UNEMPLOY / CPIAUCSL

# Lump-sum transfer (government social insurance payments, net of UI, real,
# per capita)

# 2019-04-06: Noticed that tau.lump is computed above, with conversion to
# real using CPI. The nominal to real conversion seems to be missing below,
# so I commented out that line.

#tau.lump <- (A063RC1 - W825RC1) / CNP16OV

t.start = start(tau.lump)
t.end = end(tau.lump)

y_ <- hpfilter(tau.lump, type="lambda", freq=lambda.high*lambda.q2m)
tau.lump.dt <- xts(y_$cycle, order.by=seq(as.Date(t.start),as.Date(t.end),by='month'))
y_ <- hpfilter(tau.lump.dt, type="lambda", freq=lambda.low*lambda.q2m)
tau.lump.sm <- xts(y_$trend, order.by=seq(as.Date(t.start),as.Date(t.end),by='month'))

### Could not get satisfactory output from any of the Baxter-King or
### Christiano-Fitzgerald band pass filters.  Write raw data and massage with
### Matlab code instead.

g.y.ratio <- GCE / GDP
print("Mean g-y ratio")
print(mean(as.xts(g.y.ratio)[paste('2005-01-01','2007-12-01',sep='/')]))

print("Mean debt-output ratio, 2005m1-2007m12")
print(mean(as.xts(debt.y)[paste('2005-01-01','2007-12-01',sep='/')]))

print("Mean of g/y, 2000-12 to 2007-12")
print(mean(as.xts(g.y.ratio)[paste('2000-12-01','2007-12-01',sep='/')]))
print("Mean of d/y, 2000-12 to 2007-12")
print(mean(as.xts(debt.y)[paste('2000-12-01','2007-12-01',sep='/')]))

smooth.month.b <- month.spline.smooth(b)


### New separation and job-filling probabilities

raw <- read.csv('feds200434.csv')
EUhaz <- ts(raw$EUhaz, start=c(1995,9), frequency=12)
UEhaz <- ts(raw$UEhaz, start=c(1995,9), frequency=12)

dt.start <- start(EUhaz)
dt.end <- end(EUhaz)

yr <- seq(dt.start[1]+(dt.start[2]-1)/12, dt.end[1]+(dt.end[2]-1)/12, 1/12)
#EUhaz.spl <- smooth.spline(yr, EUhaz, lambda=.0001)
#EUhaz.sp <- predict(EUhaz.spl, yr)
#EUhaz.smooth <- ts(EUhaz.sp$y, start=dt.start, frequency=12)

#UEhaz.spl <- smooth.spline(yr, UEhaz, lambda=.0001)
#UEhaz.sp <- predict(UEhaz.spl, yr)
#UEhaz.smooth <- ts(UEhaz.sp$y, start=dt.start, frequency=12)

EUhaz.smooth <- hpfilter(EUhaz, type="lambda", freq=lambda.low*lambda.q2m)$trend
UEhaz.smooth <- hpfilter(UEhaz, type="lambda", freq=lambda.low*lambda.q2m)$trend

fmean <- mean(as.xts(UEhaz)[paste('2000-12-01','2007-12-01',sep='/')])
smean <- mean(as.xts(EUhaz)[paste('2000-12-01','2007-12-01',sep='/')])
umean <- mean(UNRATE[paste('2000-12-01','2007-12-01',sep='/')])
vumean <- mean(vu[paste('2000-12-01','2007-12-01',sep='/')])

print("")
print("FRB")
print("Mean job-separation prob., 2000m12-2007m12")
print(smean)
print("Mean job-finding prob., 2000m12-2007m12")
print(fmean)
print("Mean unemployment rate (%), 2000m12-2007m12")
print(umean)
print("Mean v-u ratio, 2000m12-2007m12")
print(vumean)

#############################################################################

dt.start <- start(cps.s)
dt.end <- end(cps.s)

yr <- seq(dt.start[1]+(dt.start[2]-1)/12, dt.end[1]+(dt.end[2]-1)/12, 1/12)
#cps.s.spl <- smooth.spline(yr, cps.s, lambda=.0000001)
#cps.s.sp <- predict(cps.s.spl, yr)
#cps.s.smooth <- ts(cps.s.sp$y, start=dt.start, frequency=12)
cps.s.smooth <- hpfilter(cps.s, type="lambda", freq=lambda.low*lambda.q2m)$trend

#cps.f.spl <- smooth.spline(yr, cps.f, lambda=.0000001)
#cps.f.sp <- predict(cps.f.spl, yr)
#cps.f.smooth <- ts(cps.f.sp$y, start=dt.start, frequency=12)
cps.f.smooth <- hpfilter(cps.f, type="lambda", freq=lambda.low*lambda.q2m)$trend

fmean <- mean(as.xts(cps.f)[paste('2000-12-01','2007-12-01',sep='/')])
smean <- mean(as.xts(cps.s)[paste('2000-12-01','2007-12-01',sep='/')])
umean <- mean(UNRATE[paste('2000-12-01','2007-12-01',sep='/')])
vumean <- mean(vu[paste('2000-12-01','2007-12-01',sep='/')])

print("")
print("CPS")
print("Mean job-separation prob., 2000m12-2007m12")
print(smean)
print("Mean job-finding prob., 2000m12-2007m12")
print(fmean)
print("Mean unemployment rate (%), 2000m12-2007m12")
print(umean)
print("Mean v-u ratio, 2000m12-2007m12")
print(vumean)

fmean <- mean(as.xts(cps.f)[paste('2005-12-01','2007-12-01',sep='/')])
smean <- mean(as.xts(cps.s)[paste('2005-12-01','2007-12-01',sep='/')])
umean <- mean(UNRATE[paste('2005-12-01','2007-12-01',sep='/')])
vumean <- mean(vu[paste('2005-12-01','2007-12-01',sep='/')])

print("")
print("CPS")
print("Mean job-separation prob., 2005m12-2007m12")
print(smean)
print("Mean job-finding prob., 2005m12-2007m12")
print(fmean)
print("Mean unemployment rate (%), 2005m12-2007m12")
print(umean)
print("Mean v-u ratio, 2005m12-2007m12")
print(vumean)

print("")
print("GR facts")
print("Mean job-separation prob., 2007m01-2007m12")
print(mean(as.xts(cps.s)[paste('2007-01-01','2007-12-01',sep='/')]))
print("Mean job-separation prob., 2008m01-2009m6")
print(mean(as.xts(cps.s)[paste('2008-01-01','2009-06-01',sep='/')]))
print("Mean job-finding prob., 2007m01-2007m12")
print(mean(as.xts(cps.f)[paste('2007-01-01','2007-12-01',sep='/')]))
print("Mean job-finding prob., 2008m01-2009m6")
print(mean(as.xts(cps.f)[paste('2008-01-01','2009-06-01',sep='/')]))
print("Mean job-finding prob., 2010m01-2010m12")
print(mean(as.xts(cps.f)[paste('2010-01-01','2010-12-01',sep='/')]))


#########################################################################
### Write data to file
#########################################################################

monthly.data <- merge(USREC,
                      Omega,
                      Omega.smooth, 
                      smooth.month.tau.n,
                      smooth.month.tau.c,
                      smooth.month.tau.k,
                      as.zoo(cps.s),
                      ssmooth = as.zoo(cps.s.smooth),
                      as.zoo(cps.f),
                      fsmooth = as.zoo(cps.f.smooth),
                      as.zoo(b),
                      as.zoo(smooth.month.b),
                      UNRATE,
                      ucps,
                      ujolts,
                      EUhaz,
                      UEhaz,
                      jolts.s,
                      jolts.f,
                      v,
                      jolts.openings,
                      equity_premium,
                      as.zoo(tau.lump),
                      as.zoo(tau.lump.dt),
                      as.zoo(tau.lump.sm)
                      )
names(monthly.data) <- c("USREC",
                         "Omega",
                         "Omega.smooth",
                         "tau_w",
                         "tau_c",
                         "tau_k",
                         "cps.s",
                         "cps.s.smooth",
                         "cps.f",
                         "cps.f.smooth",
                         "b",
                         "b.smooth",
                         "UNRATE",
                         "ucps",
                         "ujolts",
                         "EUhaz",
                         "UEhaz",
                         "jolts.s",
                         "jolts.f",
                         "v",
                         "jolts.openings",
                         "equity.premium",
                         "tau.lump",
                         "tau.lump.dt",
                         "tau.lump.sm")

monthly.data <- monthly.data["2004-07/2019-01"]

write.zoo(monthly.data, file='us-monthly.txt', col.names=FALSE, sep=' ', na='?')

write.zoo(merge(monthly.data$Omega.smooth,
                monthly.data$tau_w,
                monthly.data$tau_c,
                monthly.data$tau_k,
                monthly.data$cps.s.smooth,
                monthly.data$cps.f.smooth,
                1000000*monthly.data$b.smooth,
                monthly.data$tau.lump.sm),
          file='us-monthly.dat', col.names=FALSE, sep=' ', na='?')


###dat <- merge(u, e, cps.s, cps.f, s.filt, f.filt, s.hp$trend, f.hp$trend,
###             jolts.s, jolts.f, UNRATE, jolts.s.hp$trend, jolts.f.hp$trend,
###             v, v.hp$trend, jolts.openings, jolts.openings.hp$trend,
###             100*ucps, 100*ujolts, e.hp$trend, e.hp$cycle, jolts.s.smooth, jolts.f.smooth)
###
###write.zoo(dat, file='fredx.dat', row.names=FALSE, sep='\t', na='?')
###write.zoo(dat, file='fredx.csv', row.names=FALSE, sep=',', na='')

###quarterly.data <- merge(y = as.zoo(ts(y.pc, start=c(1948,1), frequency=4)),
###                        c = as.zoo(ts(c.pc, start=c(1948,1), frequency=4)),
###                        h = as.zoo(ts(h.pc, start=c(1948,1), frequency=4)),
###                        x = as.zoo(ts(x.pc, start=c(1948,1), frequency=4)),
###                        g = as.zoo(ts(g.pc, start=c(1948,1), frequency=4)),
###                        debt = as.zoo(ts(debt.y, start=c(1966,1), frequency=4)),
###                        surplus = as.zoo(ts(surplus.y, start=c(1959,3), frequency=4)),
###                        as.zoo(ts(real.wage, start=c(1979,1), frequency=4)),
###                        quarter_tau_n, quarter_tau_c, quarter_tau_k)

quarterly.data <- merge(y.pc, c.pc, h.pc, x.pc, g.pc, debt.y, surplus.y, real.wage, quarter_tau_n, quarter_tau_c, quarter_tau_k, quarter_solow_residual)

write.zoo(quarterly.data, file='us-quarterly.csv', col.names=FALSE, row.names=FALSE, sep=',', na='NaN')

### New for April 2019

quarterly.data <- quarterly.data[complete.cases(quarterly.data)]
t.start <- start(quarterly.data)
t.end <- end(quarterly.data)

quarterly.dt <- xts(, order.by=seq(as.Date(t.start),as.Date(t.end),by='quarter'))
quarterly.sm <- xts(, order.by=seq(as.Date(t.start),as.Date(t.end),by='quarter'))

for (i in 1:dim(quarterly.data)[2])
{
  y_ <- hpfilter(quarterly.data[,i], type="lambda", freq=lambda.high)
  quarterly.dt <- merge(quarterly.dt, xts(y_$cycle, order.by=seq(as.Date(t.start),as.Date(t.end),by='quarter')))
  y_ <- hpfilter(y_$cycle, type="lambda", freq=lambda.low)
  quarterly.sm <- merge(quarterly.sm, xts(y_$trend, order.by=seq(as.Date(t.start),as.Date(t.end),by='quarter')))
}

dnames <- c("y", "c", "h", "x", "g", "debt", "surplus", "realwage", "tau_w", "tau_c", "tau_k", "solow")

### Raw, logged per capita data
names(quarterly.data) <- dnames
### Data detrended with a very smooth trend line
names(quarterly.dt) <- dnames
### Smoothed version of detrended data
names(quarterly.sm) <- dnames

write.zoo(merge(100*quarterly.data$y, 100*quarterly.dt$y, 100*quarterly.sm$y,  
                100*quarterly.data$c, 100*quarterly.dt$c, 100*quarterly.sm$c,  
                100*quarterly.data$h, 100*quarterly.dt$h, 100*quarterly.sm$h,  
                100*quarterly.data$x, 100*quarterly.dt$x, 100*quarterly.sm$x,  
                100*quarterly.data$g, 100*quarterly.dt$g, 100*quarterly.sm$g,  
                quarterly.data$debt, quarterly.dt$debt, quarterly.sm$debt,  
                100*quarterly.data$surplus, quarterly.dt$surplus, quarterly.sm$surplus,  
                quarterly.data$realwage, quarterly.dt$realwage, quarterly.sm$realwage,  
                quarterly.data$tau_w, quarterly.data$tau_c, quarterly.data$tau_k,  
                quarterly.data$solow, quarterly.dt$solow, quarterly.sm$solow), 
          file='us-quarterly.txt', col.names=FALSE, sep=' ', na='?')
                
write.zoo(merge(quarterly.sm$y, quarterly.sm$g), 
          file='us-quarterly.dat', col.names=FALSE, sep=' ', na='?')

print("Quarterly")
print(acf(quarterly.sm$g, plot=FALSE, lag.max=1))
print(acf(quarterly.sm$tau_k, plot=FALSE, lag.max=1))
print(acf(quarterly.sm$tau_c, plot=FALSE, lag.max=1))
print(acf(quarterly.sm$tau_w, plot=FALSE, lag.max=1))
print(acf(quarterly.sm$solow, plot=FALSE, lag.max=1))

#print(acf(monthly.data$b.smooth, plot=FALSE, lag.max=1))
#print(acf(monthly.data$cps.s.smooth, plot=FALSE, lag.max=1))

ar.y <- (acf(quarterly.sm$y, plot=FALSE, lag.max=1)$acf[2])^(1/3)
ar.g <- (acf(quarterly.sm$g, plot=FALSE, lag.max=1)$acf[2])^(1/3)
ar.tau_k <- (acf(quarterly.sm$tau_k, plot=FALSE, lag.max=1)$acf[2])^(1/3)
ar.tau_c <- (acf(quarterly.sm$tau_c, plot=FALSE, lag.max=1)$acf[2])^(1/3)
ar.tau_w <- (acf(quarterly.sm$tau_w, plot=FALSE, lag.max=1)$acf[2])^(1/3)
ar.solow <- (acf(quarterly.sm$solow, plot=FALSE, lag.max=1)$acf[2])^(1/3)

ar.b <- acf(monthly.data$b.smooth, plot=FALSE, lag.max=1)$acf[2]
ar.s <- acf(monthly.data$cps.s.smooth, plot=FALSE, lag.max=1)$acf[2]
ar.f <- acf(monthly.data$cps.f.smooth, plot=FALSE, lag.max=1)$acf[2]
ar.Omega <- acf(Omega.smooth, plot=FALSE, lag.max=1)$acf[2]

print("Monthly autoregressive coefficients")
print("y")
print(ar.y)
print("g")
print(ar.g)
print("tau_k")
print(ar.tau_k)
print("tau_c")
print(ar.tau_c)
print("tau_w")
print(ar.tau_w)
print("solow")
print(ar.solow)

print("b")
print(ar.b)
print("s")
print(ar.s)
print("f")
print(ar.f)
print("Omega")
print(ar.Omega)


sink(file=NULL)

