###source('usdata.r') ### Obtain from http://paulgomme.github.io

### May need to install a number of packages:
###
### install.packages('quantmod')
### install.packages('Hmisc')
### install.packages('mFilter')
### install.packages('FRAPO')

fredsym <- c(
    "FII20",	# 20-Year Treasury Inflation-Indexed Security, Constant Maturity
    "GCE", # Government expenditures on consumption and investment
    "GDP", # Gross Domestic Product
    "A822RE1Q156NBEA", # Shares of gross domestic product: Government consumption expenditures and gross investment
    "GDPC1", # Real Gross Domestic Product, 3 Decimal
    "CNP16OV", # Civilian Noninstitutional Population
    "GFDEGDQ188S", #  Federal Debt: Total Public Debt as Percent of Gross Domestic Product
    "GFDGDPA188S") # Gross Federal Debt as Percent of Gross Domestic Product

for (i in 1:length(fredsym))
  {
    getSymbols(fredsym[i],src="FRED")
  }

Rk <- 100*((1+quarter_return_business_capital_after_tax/100)^(1/12)-1)
Rb <- 100*((1+FII20/100)^(1/12)-1)

options(digits=20)

print("Monthly return, business capital, after tax, 2018Q1")
print(Rk[paste('2018-01-01')])
print("Monthly return, business capital, after tax, 2010Q1-2018Q1")
print(mean(Rk[paste('2010-01-01','2018-01-01',sep='/')]))

print("TIIS, 2019")
print(mean(Rb[paste('2019-01-01','2019-12-01',sep='/')]))
print("TIIS, 2010-2019")
print(mean(Rb[paste('2010-01-01','2019-12-01',sep='/')]))

print("Mean of tau_n, 2018")
print(mean(quarter_tau_n[paste('2018-01-01','2018-12-01',sep='/')]))
print("Mean of tau_n, 2019")
print(mean(quarter_tau_n[paste('2019-01-01','2019-12-01',sep='/')]))
print("Mean of tau_k, 2016")
print(mean(quarter_tau_k[paste('2016-01-01','2016-12-01',sep='/')]))
print("Mean of tau_k, 2017")
print(mean(quarter_tau_k[paste('2017-01-01','2017-12-01',sep='/')]))
print("Mean of tau_k, 2018")
print(mean(quarter_tau_k[paste('2018-01-01','2018-12-01',sep='/')]))
print("Mean of tau_k, 2019")
print(mean(quarter_tau_k[paste('2019-01-01','2019-12-01',sep='/')]))

pop <- aggregate(CNP16OV, as.yearqtr, mean) / 1000
y <- GDPC1
y.pc <- log(y / as.xts(pop))

debt.y <- GFDEGDQ188S

g.y.ratio <- GCE / GDP
print("Mean g-y ratio, 2018m1-2018m12")
print(mean(as.xts(g.y.ratio)[paste('2018-01-01','2018-12-01',sep='/')]))
print("Mean g-y ratio, 2019m1-2019m12")
print(mean(as.xts(g.y.ratio)[paste('2019-01-01','2019-12-01',sep='/')]))

print("Mean g-y ratio, 2019m1-2019m12")
print(mean(as.xts(A822RE1Q156NBEA)[paste('2019-01-01','2019-12-01',sep='/')]))

print("Mean debt-output ratio, 2018m1-2018m12")
print(mean(as.xts(debt.y)[paste('2018-01-01','2018-12-01',sep='/')]))
print("Mean debt-output ratio, 2019m1-2019m12")
print(mean(as.xts(debt.y)[paste('2019-01-01','2019-12-01',sep='/')]))


dep.var <- y.pc[paste('2010-01-01','2019-12-01',sep='/')]
trend <- seq(1, length(dep.var))
lm_results <- lm(dep.var ~ trend)

yhat = c()
for (i in 1:44)
{
  yhat[i] <- lm_results$coefficients[1] + i * lm_results$coefficients[2]
}

y_short <- y.pc[paste('2010-01-01','2020-10-01',sep='/')]

y.dev <- (y_short - yhat)

write.table(y.dev[paste('2020-01-01','2020-12-01',sep='/')], file='corona_q.dat', col.names=FALSE, row.names=FALSE, sep=' ', na='-99', quote=FALSE)
