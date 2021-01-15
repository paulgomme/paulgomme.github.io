### This file created by Paul Gomme is licensed under a Creative Commons
### Attribution 4.0 International License:
### https://creativecommons.org/licenses/by/4.0/

### Generate data for: Gomme, Ravikumar and Rupert (2011), "The
### Return to Capital and the Business Cycle", Review of Economic Dynamics;
### Gomme and Rupert (2007), "Theory, Measurement, and Calibration
### of Macroeconomic Models", Journal of Monetary Economics; and/or
### Gomme and Lkhagvasuren (2013), "Calibration and
### Simulation of DSGE Models", Handbook of Empirical Methods in
### Macroeconomics.

### Running this file will generate 3 files: us-moments.tex (a table of
### second moments of `standard' macroaggregates); usdata.csv (the data used
### to generate these second moments); and shocks.csv (updates of the shock
### series used in Gomme and Rupert (2007).

### If you use this data, kindly cite one (or more) of these papers. BibTeX
### entries:
###
###@article{grr:returns,
###  author	= {Paul Gomme and B. Ravikumar and Peter Rupert},
###  title	= {The Return to Capital and the Business Cycle},
###  journal	= {Review of Economic Dynamics},
###  year	= {2011},
###  volume	= {14},
###  pages	= {262--278},
###  number	= {2},
###  month	= apr
###}
###@article{gomme/rupert:guide,
###  author	= {Paul Gomme and Peter Rupert},
###  title	= {Theory, Measurement, and Calibration of Macroeconomic
###		  Models},
###  journal	= {Journal of Monetary Economics},
###  year	= {2007},
###  volume	= {54},
###  pages	= {460--497},
###  number	= {2},
###  month	= mar
###}
###
###@InCollection{Gomme/Lkhagvasuren:calibration,
###  author	= {Paul Gomme and Damba Lkhagvasuren},
###  title	= {Calibration and Simulation of DSGE Models},
###  booktitle	= {Handbook of Empirical Methods in Macroeconomics},
###  publisher	= {Edward Elgar},
###  year	= {2013},
###  editor	= {Nigar Nasimzade and Michael Thornton},
###  pages	= {575--592}
###}
###
### History:
###
### Programming by Paul Gomme.
###
### The original work, for Gomme and Rupert (2007), was done in Microsoft
### Excel. At the time, I (Paul Gomme) worked at the Federal Reserve Bank of
### Cleveland where I had access to Haver Analytics data which was accessed
### through Excel. Consequently, it seemed natural to keep all the data work
### within Excel.
###
### However, when trying to maintain the data and update it for subsequent
### work, Excel proved to be a nightmare. Links to columns made no sense
### since they were not descriptive of the data being linked to. I tried use
### tabs to organize the data, but this just meant that tracing back to the
### definition of variables was even harder. To make it easier to maintain
### the data, I eventually switched to Matlab. This was workable and I could
### assign meaningful names to variables so that the various data
### manipulations are somewhat more transparent. However, I still needed
### someone at a Federal Reserve Bank to help out every so often updating an
### Excel spreadsheet withe the raw data. So, there was a 2 step process:
### update the data in Excel, then bring the data into Matlab.  The second
### operation is easier under Microsoft Windows, and only a bit more
### complicated under Linux (my preferred operating system since 2008).
###
### More recently, B. Ravikumar encouraged the maintainers of the St. Louis
### Fed's FRED to put up the data needed for this work. Translation of the
### Matlab code to R ensured and this file is the end product. Thanks to Lin
### Shao, then of the St. Louis Fed, for figuring out most of the FRED codes
### corresponding to the data below.

### R can be downloaded from https://www.r-project.org/

### A friendly GUI for R is available at https://www.rstudio.com/home/

### Kindly report any errors to paul.gomme@concordia.ca

### May need to install a number of packages:
###
### install.packages('curl') ! 2017-06-12
### install.packages('quantmod')
### install.packages('Hmisc') ! 2016-12-21: having problems downloading and installing
###    Evidently, Hmisc isn't needed.
### install.packages('mFilter')
### install.packages('FRAPO') ! Not necessary?
### install.packages('nleqslv') ! Added 2016-12-21

### 2018-08-30 The mFilter package is "missing" at CRAN.  There are two
### work-arounds.  The first is to visit
### https://github.com/cran/mFilter/blob/master/R/hpfilter.R and
### copy-and-paste the contents to the file hpfilter.R
### Then, in R:
###     source("hpfilter.R")
###
### The second is to "force" CRAN to let you install the "old" version of
### mFilter.  Doing so is a bit involved.  I use Linux Mint and needed the
### ssl libraries.  From a command prompt, run:
###     sudo apt install libssl-dev
### Then, in R:
###     install.packages("openssl")
###     install.packages("git2r")
###     install.packages("httr")
###     install.packages("devtools")
### Notice that the part really needed is "devtools", but devtools depends on
### the other packages (the first of which require libssl-dev).  If you have
### already installed devtools, then skip the install.packages above.  Finally:
###     require(devtools)
###     install_version("mFilter", version="0.1-3", repos="http://cran.us.r-project.org")
### Hopefully someone will notice that mFilter is actually used and fix this problem.

rm(list=ls())

library("quantmod")
library(zoo)
###library(Hmisc)
library(mFilter)
library(nleqslv)

### The annual data

fredsym <- c(
    "A2007C1A027NBEA",	# Housing Output (Bil.$)
    "A2009C1A027NBEA",	# Gross Housing Value Added (Bil.$)
    "B952RC1A027NBEA",	# Net Housing Value Added (Bil.$)
    "B1033C1A027NBEA",	# Housing: Compensation of Employees (Bil.$)
    "B1031C1A027NBEA",	# Housing: Taxes on Production and Imports (Bil.$)
    "W154RC1A027NBEA",	# Housing Subsidies (Bil.$)
    "W165RC1A027NBEA",	# Housing Net Operating Surplus (Bil.$)
    "B1037C1A027NBEA",	# Housing: Net Interest (Bil.$)
    "W166RC1A027NBEA",	# Housing Net Operating Surplus: Current Transfer Payments (Bil.$)
    "B1034C1A027NBEA",	# Housing: Proprietors' Income with IVA & CCAdj (Bil.$)
    "B1035C1A027NBEA",	# Housing: Rental Income of Persons w/CCadj (Bil.$)
    "B1036C1A027NBEA",	# Housing: Corporate Profits w/IVA & CCadj (Bil.$)
    "W153RC1A027NBEA",	# Housing Net Operating Surplus: Current Surplus of Govt Enterprises (Bil.$)
### See discussion preceeding calculation of quarter_tau_k
###    "S230401A027NBEA",	# State/Local Govt: Other Taxes (Bil.$)
    "B235RC1A027NBEA",	# Federal government current tax receipts: Taxes on production and imports: Customs duties
    "B234RC1A027NBEA",	# Federal government current tax receipts: Taxes on production and imports: Excise taxes
    "ASLSTAX", 		# State and Local Government: Taxes on production and imports: Sales Taxes
    "A033RC1A027NBEA",	# National income: Compensation of employees
    #"W209RC1A027NBEA",	# Compensation of Employees (Bil.$) <-- DISCONTINUED; use previous entry
    "A553RC1A027NBEA",	# Government Wages and Salaries (Bil.$) *** DISCONTINUED; use next entry
    "B202RC1A027NBEA",	# Government Wages and Salaries (Bil.$)
    "A048RC1A027NBEA",	# Rental Income of Persons with CCAdj (Bil.$)
    "A051RC1A027NBEA",	# Corporate Profits with IVA and CCAdj (Bil.$)
    "W255RC1A027NBEA",	# Net Interest and Miscellaneous Payments (Bil.$)
    "B1035C1A027NBEA",	# Housing: Rental Income of Persons w/CCadj (Bil.$)
    "B1036C1A027NBEA",	# Housing: Corporate Profits w/IVA & CCadj (Bil.$)
    "B1037C1A027NBEA",	# Housing: Net Interest (Bil.$)
    "GNPA",		# Gross National Product (Bil.$)
    "GDPA",		# Gross Domestic Product (Bil.$)
    "A765RC1A027NBEA",	# Gross Value Added: General Government (Bil.$)
    "A027RC1A027NBEA",	# Net National Product (Bil.$)
    "A194RC1A027NBEA",	# General Government: Net Domestic Product (Bil.$)
    "K1NTOTL1EQ000",	# Net Stock: Private Fixed Nonresidential Equipment (Bil.$)
    "K1NTOTL1ST000",	# Net Stock: Private Fixed Nonresidential Structures (Bil.$)
    "K1R53101ES000",	# Net Stock: Private Residential Fixed Assets (Bil.$)
    "K1CTOTL1CD000",	# Net Stock: Consumer Durable Goods (Bil.$)
    "K1GTOTL1EQ000",	# Net Stock: Government Nonresidential Equipment (Bil.$)
    "K1GTOTL1STNR0",	# Net Stock: Govt Nonresidential Structures (Bil.$)
    "K1GTOTL1SA000",	# Net Stock: Government Residential Fixed Assets (Bil.$)
    "PCNDA", 		# Personal Consumption Expenditures: Nondurable Goods (Bil.$)
    "DNDGRG3A086NBEA",	# Personal consumption expenditures: Nondurable goods (chain-type price index)
    "PCESVA",		# Personal Consumption Expenditures: Services (Bil.$)
    "DSERRG3A086NBEA",	# Personal Consumption Expenditures: Services (chain-type price index)
    "K100071A027NBEA",	# Produced Assets Net Stock: Private Inventories (Bil.$)
    "K160421A027NBEA",	# Neutral Holding Gains or Losses[-]: Private Inventories (Bil.$)
    "K160471A027NBEA",	# Real Holding Gains or Losses[-]: Private Inventories (Bil.$)
    "M1NTOTL1EQ000",	# Depreciation: Pvt Nonres Fxd Assets: Equip (Bil.$)
    "M1NTOTL1ST000",	# Depreciation: Pvt Nonres Fxd Assets: Struct (Bil.$)
    "M1R53101ES000",	# Depreciation: Res Fixed Assets: Private (Bil.$)
    "M1GTOTL1EQ000",	# Depreciation: Gov Nonres Fixed Assets: Equipment (Bil.$)
    "M1GTOTL1STNR0",	# Depreciation: Gov Nonresidential Fixed Assets: Structures (Bil.$)
    "M1GTOTL1SA000",	# Depreciation: Res Fixed Assets: Government (Bil.$)
    "M1CTOTL1CD000",	# Depreciation: Consumer Durable Goods (Bil.$)
    "A553RC1A027NBEA",	# Government Wages and Salaries (Bil.$) # duplicate??
    "A955RC1A027NBEA",	# Government consumption expenditures
    "B009RC1A027NBEA",	# Private Nonresidential Investment: Structures (Bil.$)
    "Y033RC1A027NBEA",	# Private Nonresidential Fixed Investment: Equipment (Bil.$)
    "S210401A027NBEA",	# Personal current taxes: State and local: Property taxes
    "ASLPTAX",		# State and Local Government: Taxes on production and imports: Property Taxes
    "PRFIA",		# Private Residential Investment (Bil.$)
    "PCDGA",		# Personal Consumption Expenditures: Durable Goods (Bil.$)
    "PNFIA",		# Private Nonresidential Fixed Investment (Bil.$)
    "B008RG3A086NBEA",	# Real private fixed investment: Nonresidential (chain-type price index) (2009=100)
    "PNFICA" 		# Real Private Nonresidential Fixed Investment (Bil.Chn.2009$)
    )

for (i in 1:length(fredsym))
  {
    getSymbols(fredsym[i],src="FRED")
  }

### The quarterly data

fredsym <- c(
    "W055RC1Q027SBEA",	# Personal Current Taxes (SAAR, Bil.$)
    "W025RC1Q027SBEA",	# Government Tax Receipts on Corporate Income (SAAR, Bil.$)
    "B249RC1Q027SBEA",	# State & Local Government Property Tax Receipts (SAAR, Bil.$)
    "S210400",		# State & Local Property Taxes (SAAR, Mil.$)
    "B248RC1Q027SBEA",	# State & Local Sales Taxes
    "B234RC1Q027SBEA",	# Federal Government Excise Taxes
    "B235RC1Q027SBEA",	# Federal Government Custom Duties
    "GDICOMP",		# Compensation of Employees, Paid: Domestic Employers (SAAR, Bil.$)
    "A576RC1Q027SBEA",	# Wages & Salaries (SAAR, Bil.$) *** DISCONTINUED - use WASCUR
    "WASCUR",		# Wages & Salaries (SAAR, Bil.$)
    "W272RC1Q027SBEA",	# Net Interest & Miscellaneous Payments: Domestic Industries (SAAR, Bil.$)
    "PROPINC",		# Proprietors' Income with IVA and CCAdj (SAAR, Bil.$)
    "RENTIN",		# Rental Income of Persons with CCAdj (SAAR, Bil.$)
    "A445RC1Q027SBEA",	# Corporate Profits with IVA & CCAdj: Domestic Industries (SAAR, Bil.$)
    "B029RC1Q027SBEA",	# Business Current Transfer Payments (SAAR, Bil.$)
    "W260RC1Q027SBEA",	# Net Operating Surplus: Private Enterprises (SAAR, Bil.$)
    "B039RC1Q027SBEA",	# Employer Contributions for Government Social Insurance (SAAR, Bil.$)
    "A061RC1Q027SBEA",	# Contributions for Government Social Insurance (SAAR, Bil.$)
    "A024RC1Q027SBEA",	# Consumption of Private Fixed Capital (SAAR, Bil.$)
    "W276RC1Q027SBEA",	# Consumption of Fixed Capital: Domestic Business (Bil.$)
    "CBI",		# Change in Private Inventories (SAAR, Bil.$)
    "A2009C1Q027SBEA",	# Gross Housing Value Added (SAAR, Bil.$)
#    "A024RC1Q027SBEA", 	# Consumption of fixed capital: Private
    "B2607C1Q027SBEA",	# Consumption of fixed capital: Private: Households and institutions: Owner-occupied housing
    "USRECQ",		# Quarterly NBER Recession/Expansion: Recession Shading (+1/-1)
    "CPIAUCSL",		# 	CPI-U: All Items (SA, 1982-84=100) (monthly)
    "CNP16OV",		# Civilian Noninstitutional Population: 16 Years and Over (NSA, Thous) (monthly)
    "PCND",		# Personal Consumption Expenditures: Nondurable Goods (SAAR, Bil.$)
    "DNDGRG3Q086SBEA",	# Personal consumption expenditures: Nondurable goods (chain-type price index), 2009-100
    "PCESV",		# Personal Consumption Expenditures: Services (SAAR, Bil.$)
    "DSERRG3Q086SBEA",	# Personal consumption expenditures: Services (chain-type price index), 2009=100
    "GDP",		# Gross Domestic Product (SAAR, Bil.$)
    "DHUTRC1Q027SBEA",	# 	PCE: Housing Services and utilities (starts 1959I)
    "A553RC1Q027SBEA",	# Government Wages and Salaries (SAAR, Bil.$) *** DISCONTINUED; use next entry?
    "B202RC1Q027SBEA",	# Government Wages and Salaries (Bil.$)
    "A955RC1Q027SBEA",	# Government consumption expenditures
#    "DHSGRC1A027NBEA",	# (only annual data)	Personal Consumption Expenditures: Housing (SAAR, Mil.$)
    "B009RC1Q027SBEA",	# Private Nonresidential Investment: Structures (SAAR, Bil.$)
    "Y033RC1Q027SBEA",	# Private Nonresidential Fixed Investment: Equipment (SAAR, Bil.$)
    "PRFI",		# Private Residential Investment (SAAR,Bil.$)
    "PCDG",		# Personal Consumption Expenditures: Durable Goods (SAAR, Bil.$)
    "PRSCQ",		# Aggregate Hours: Nonfarm Payrolls, Private Sector (SAAR, Bil.Hrs)
    "A760RC1Q027SBEA",	# Government Gross Investment in Structures (SAAR, Bil.$)
    "Y054RC1Q027SBEA",	# Government Gross Investment in Equipment (SAAR, Bil.$)
    "PNFI",		# Private Nonresidential Fixed Investment
    "B008RG3Q086SBEA",	# Real private fixed investment: Nonresidential (chain-type price index), 2009=100
    "A371RC1Q027SBEA"	# (since 1996; splice in previous Haver data?)	Private Inventories (EOP, SAQT, Bil.$)
    )

for   (i in 1:length(fredsym))
  {
    getSymbols(fredsym[i],src="FRED")
  }


annual.housing_output <- A2007C1A027NBEA
annual.gross_housing_value_added <- A2009C1A027NBEA
annual.net_housing_value_added <- B952RC1A027NBEA
annual.housing_compensation_of_employees <- B1033C1A027NBEA
annual.housing_taxes_on_production <- B1031C1A027NBEA
annual.housing_subsidies <- W154RC1A027NBEA
annual.housing_net_operating_surplus <- W165RC1A027NBEA
annual.housing_net_interest <- B1037C1A027NBEA
annual.housing_NOS_transfer_payments <- W166RC1A027NBEA
annual.housing_proprietors_income <- B1034C1A027NBEA
annual.housing_rental_income <- B1035C1A027NBEA
annual.housing_corp_profits <- B1036C1A027NBEA
annual.housing_NOS_gov_enterprises <- W153RC1A027NBEA
### 2019-07-03 See discussion preceeding quarter_tau_k
###annual.state_local_gov_other_taxes <- S230401A027NBEA
annual.state_local_gov_property_taxes <- ASLPTAX
annual.state_local_property_taxes <- S210401A027NBEA
annual.state_local_sales_taxes <- ASLSTAX
annual.gov_excise_taxes <- B234RC1A027NBEA
annual.gov_custom_duties <- B235RC1A027NBEA
#annual.compensation_of_employees <- W209RC1A027NBEA
annual.compensation_of_employees <- A033RC1A027NBEA #2018-05-12
#annual.gov_wages_and_salaries <- A553RC1A027NBEA
annual.gov_wages_and_salaries <- B202RC1A027NBEA # 2018-03-02
quarter.gov_consumption_expenditures <- A955RC1Q027SBEA
annual.rental_income <- A048RC1A027NBEA
annual.corporate_profits <- A051RC1A027NBEA
annual.net_interest <- W255RC1A027NBEA
annual.housing_rental_income <- B1035C1A027NBEA
annual.housing_corp_profits <- B1036C1A027NBEA
annual.housing_net_interest <- B1037C1A027NBEA
annual.GNP <- GNPA
annual.GDP <- GDPA
annual.gross_value_added_gov <- A765RC1A027NBEA
annual.net_national_product <- A027RC1A027NBEA
annual.gov_net_national_product <- A194RC1A027NBEA
annual.net_stock_private_equipment_software <- K1NTOTL1EQ000
annual.net_stock_private_nonresidential_structures <- K1NTOTL1ST000
annual.net_stock_private_residential_structures <- K1R53101ES000
annual.net_stock_consumer_durables <- K1CTOTL1CD000
annual.net_stock_gov_nonresidential_equipment_software <- K1GTOTL1EQ000
annual.net_stock_gov_nonresidential_structures <- K1GTOTL1STNR0
annual.net_stock_gov_residential_structures <- K1GTOTL1SA000
annual.real_PCE_nondurables <- PCNDA * 100 / DNDGRG3A086NBEA
annual.real_PCE_services <- PCESVA * 100 / DSERRG3A086NBEA
annual.PCE_nondurables <- PCNDA
annual.PCE_services <- PCESVA
annual.private_inventories <- K100071A027NBEA
annual.private_inventories_neutral_holding_gains_losses <- K160421A027NBEA
annual.private_inventores_real_holding_gains_losses <- K160471A027NBEA
annual.depreciation_private_equipment_software <- M1NTOTL1EQ000
annual.depreciation_private_nonresidential_structures <- M1NTOTL1ST000
annual.depreciation_private_residential_structures <- M1R53101ES000
annual.depreciation_gov_equipment_software <- M1GTOTL1EQ000
annual.depreciation_gov_nonresidential_structures <- M1GTOTL1STNR0
annual.depreciation_gov_residential_structures <- M1GTOTL1SA000
annual.depreciation_consumer_durables <- M1CTOTL1CD000
annual.private_investment_nonresidential_structures <- B009RC1A027NBEA
annual.private_investment_equipment_software <- Y033RC1A027NBEA
annual.private_investment_residential_structures <- PRFIA
annual.PCE_durables <- PCDGA
annual.private_nonresidential_fixed_investment <- PNFIA
annual.real_private_nonresidential_fixed_investment <- PNFICA
annual_deflator_investment <- B008RG3A086NBEA
annual.gov_consumption_expenditures <- A955RC1A027NBEA
#############################################################################

quarter.personal_taxes <- W055RC1Q027SBEA
quarter.gov_taxes_corp_income <- W025RC1Q027SBEA
quarter.state_local_gov_property_taxes <- B249RC1Q027SBEA
quarter.state_local_property_taxes <- S210400
quarter.state_local_sales_taxes <- B248RC1Q027SBEA
quarter.gov_excise_taxes <- B234RC1Q027SBEA
quarter.gov_custom_duties <- B235RC1Q027SBEA
quarter.compensation_of_employees <- GDICOMP
#quarter.wage_salary_accruals <- A576RC1Q027SBEA
quarter.wage_salary_accruals <- WASCUR # 2019-03-02
quarter.net_interest <- W272RC1Q027SBEA
quarter.proprietors_income <- PROPINC
quarter.rental_income <- RENTIN
quarter.corp_profits <- A445RC1Q027SBEA
quarter.business_transfer_payments <- B029RC1Q027SBEA
quarter.net_operating_surplus_private <- W260RC1Q027SBEA
quarter.employer_contributions_gov_social_insurance <- B039RC1Q027SBEA
quarter.contributions_gov_social_insurance <- A061RC1Q027SBEA
quarter.consumption_fixed_capital_business <- W276RC1Q027SBEA
quarter.change_private_inventories <- CBI
quarter.gross_housing_value_added <- A2009C1Q027SBEA
quarter.NBER <- USRECQ
quarter.PCE_nondurables <- PCND
quarter.PCE_nondurables_price <- DNDGRG3Q086SBEA
quarter.PCE_services <- PCESV
quarter.PCE_services_price <- DSERRG3Q086SBEA
quarter.real_PCE_nondurables <- PCND * 100 / DNDGRG3Q086SBEA
quarter.real_PCE_services <- PCESV * 100 / DSERRG3Q086SBEA
quarter.GDP <- GDP
quarter.PCE_housing_services <- DHUTRC1Q027SBEA
#quarter.gov_wages_and_salaries <- A553RC1Q027SBEA
quarter.gov_wages_and_salaries <- B202RC1Q027SBEA # 2018-03-02
quarter.gov_consumption_expenditures <- A955RC1Q027SBEA
quarter.private_investment_nonresidential_structures <- B009RC1Q027SBEA
quarter.private_investment_equipment_software <- Y033RC1Q027SBEA
quarter.private_investment_residential_structures <- PRFI
quarter.PCE_durables <- PCDG
quarter.hours_private_nonfarm <- PRSCQ
quarter.gov_gross_investment_structures <- A760RC1Q027SBEA
quarter.gov_gross_investment_equipment_software <- Y054RC1Q027SBEA
quarter.private_nonresidential_fixed_investment <- PNFI
quarter.real_private_nonresidential_fixed_investment_price <- B008RG3Q086SBEA
quarter.private_inventories <- A371RC1Q027SBEA
quarter.private_capital_consumption <- A024RC1Q027SBEA
quarter.private_capital_consumption_housing <- B2607C1Q027SBEA



## Monthly data; convert to quarterly.

month.CPI_all_items <- CPIAUCSL
month.civilian_noninstitutional_pop_16_plus <- CNP16OV

quarter.CPI_all_items <- aggregate(month.CPI_all_items, as.yearqtr, mean)
quarter.civilian_noninstitutional_pop_16_plus <-
  aggregate(month.civilian_noninstitutional_pop_16_plus, as.yearqtr, mean)

### FRED does not provide very long series for many real variables. Obtain
### longer data series by dividing nominal variables by the corresponding
### price index.

quarter.real_private_nonresidential_fixed_investment <-
  quarter.private_nonresidential_fixed_investment * 100 /
  quarter.real_private_nonresidential_fixed_investment_price

### Definitions of: price of consumption goods (nondurables + services); and
### the relative price of investment goods.

annual_deflator_consumption <- 100 * 
    (annual.PCE_nondurables+annual.PCE_services) / 
    (annual.real_PCE_nondurables+annual.real_PCE_services) 
annual_relative_price_investment <- annual_deflator_investment / 
    annual_deflator_consumption 

### Annual real capital stocks

annual_real_ke <- 100 * annual.net_stock_private_equipment_software / 
    annual_deflator_consumption
annual_real_ks <- 100 * annual.net_stock_private_nonresidential_structures / 
    annual_deflator_consumption
annual_real_kh <- 100 * annual.net_stock_private_residential_structures / 
    annual_deflator_consumption
annual_real_kd <- 100 * annual.net_stock_consumer_durables / 
    annual_deflator_consumption
annual_real_g_ke <- 100 * annual.net_stock_gov_nonresidential_equipment_software / 
    annual_deflator_consumption
annual_real_g_ks <- 100 * (annual.net_stock_gov_nonresidential_structures + 
 annual.net_stock_gov_residential_structures) / 
    annual_deflator_consumption

### Annual depreciation rates

annual.delta_ke <- annual.depreciation_private_equipment_software /
  lag(annual.net_stock_private_equipment_software)

annual.delta_ks = annual.depreciation_private_nonresidential_structures /
  lag(annual.net_stock_private_nonresidential_structures) 

annual.delta_kh = annual.depreciation_private_residential_structures  /
  lag(annual.net_stock_private_residential_structures) 

annual.delta_kd = annual.depreciation_consumer_durables /
  lag(annual.net_stock_consumer_durables) 

annual.delta_g_ke = annual.depreciation_gov_equipment_software  /
  lag(annual.net_stock_gov_nonresidential_equipment_software) 

annual.delta_g_ks = annual.depreciation_gov_nonresidential_structures /
  lag(annual.net_stock_gov_nonresidential_structures) 

annual.delta_g_kh = annual.depreciation_gov_residential_structures /
  lag(annual.net_stock_gov_residential_structures) 

annual.delta_m = (annual.depreciation_private_equipment_software +
                  annual.depreciation_private_nonresidential_structures) /
  (lag(annual.net_stock_private_equipment_software) + 
   lag(annual.net_stock_private_nonresidential_structures))

annual.delta_h = (annual.depreciation_consumer_durables +
                  annual.depreciation_private_residential_structures) /
  (lag(annual.net_stock_consumer_durables) + 
   lag(annual.net_stock_private_residential_structures))  

annual.delta_all = (annual.depreciation_private_equipment_software +
                    annual.depreciation_private_nonresidential_structures +
                    annual.depreciation_consumer_durables +
                    annual.depreciation_private_residential_structures) /
  (lag(annual.net_stock_private_equipment_software) + 
   lag(annual.net_stock_private_nonresidential_structures) +
   lag(annual.net_stock_consumer_durables) + 
   lag(annual.net_stock_private_residential_structures))

### Index for Korean war
tKW <- '1954-01-01'
tN = end(annual.delta_m)

#Average depreciation rates since Korean War (expressed quarterly)
delta_all_annual = mean(annual.delta_all[paste(tKW,tN,sep='/')])
delta_m_annual = mean(annual.delta_m[paste(tKW,tN,sep='/')])
delta_h_annual = mean(annual.delta_h[paste(tKW,tN,sep='/')])
delta_all = 1-(1-mean(annual.delta_all[paste(tKW,tN,sep='/')]))^(.25)
delta_m = 1-(1-mean(annual.delta_m[paste(tKW,tN,sep='/')]))^(.25)
delta_h = 1-(1-mean(annual.delta_h[paste(tKW,tN,sep='/')]))^(.25)

### Compute labor's share of income (alpha). Need to first compute factor
### incomes.

annual_income_labor = annual.compensation_of_employees - 
  annual.gov_wages_and_salaries - 
  annual.housing_compensation_of_employees
annual_income_capital = annual.rental_income + 
  annual.corporate_profits + annual.net_interest - 
  (annual.housing_rental_income + annual.housing_corp_profits + 
   annual.housing_net_interest) + annual.GNP - 
  annual.gross_value_added_gov - annual.gross_housing_value_added - 
  (annual.net_national_product - annual.gov_net_national_product - 
   annual.net_housing_value_added) 
annual_alpha <- annual_income_capital / (annual_income_capital + annual_income_labor) 
alpha_mean <- mean(annual_alpha[paste(tKW,tN,sep='/')])

### `Usual' macroaggregates

annual_real_y = annual.GDP / (annual_deflator_consumption/100)
annual_real_yp = annual_real_y - annual.gov_wages_and_salaries / 
    (annual_deflator_consumption/100) 
annual_real_xs = annual.private_investment_nonresidential_structures / 
    (annual_deflator_consumption/100) 
annual_real_xe = annual.private_investment_equipment_software / 
    (annual_deflator_consumption/100) 
annual_real_xh = annual.private_investment_residential_structures / 
    (annual_deflator_consumption/100) 
annual_real_xd = annual.PCE_durables / (annual_deflator_consumption/100)

annual.xm_y = (annual.private_investment_nonresidential_structures 
	       + annual.private_investment_equipment_software) / annual.GDP

annual.xh_y = (annual.private_investment_residential_structures 
	       + annual.PCE_durables) / annual.GDP

xm_y = mean(annual.xm_y[paste(tKW,tN,sep='/')])
xh_y = mean(annual.xh_y[paste(tKW,tN,sep='/')])

quarter_deflator_consumption = 100 * 
    (quarter.PCE_nondurables+quarter.PCE_services) / 
    (quarter.real_PCE_nondurables+quarter.real_PCE_services) 
quarter_deflator_investment = 100 * 
    quarter.private_nonresidential_fixed_investment / 
    quarter.real_private_nonresidential_fixed_investment 
quarter_relative_price_investment = quarter_deflator_investment / 
    quarter_deflator_consumption 

### Splice in some `old' data

### Data from Haver (no longer available?)

quarter_PCE_housing_services_old <- xts(c(15.1, 15.6, 16.3, 16.9, 17.3, 17.7,
	18.1, 18.5, 19.0, 19.4, 19.8, 20.3, 20.8, 21.4, 22.0, 22.6, 23.3,
	24.0, 24.7, 25.4, 26.0, 26.7, 27.2, 28.0, 28.8, 29.4, 30.3, 30.9,
	31.6, 32.1, 32.5, 33.0, 33.5, 34.1, 34.7, 35.2, 35.8, 36.4, 37.0,
	37.6, 38.3, 38.9, 39.7, 40.4, 41.1, 41.7, 42.3, 42.9, 43.7, 44.5,
	45.5, 46.4, 47.1, 47.7, 48.5, 49.4, 50.0, 50.7, 51.5, 52.5, 53.2,
	54.2, 55.2, 56.3, 56.9, 57.4, 58.3, 59.3, 60.0, 60.8, 61.8, 62.9,
	63.7, 64.8, 66.0, 67.1, 68.0, 68.8, 69.9, 71.1, 72.2, 73.2, 74.7,
	76.1, 77.7, 78.8, 80.4, 82.3, 84.0, 85.9, 87.9, 89.9, 91.5, 92.9,
	94.8, 97.2, 99.1, 101.5, 104.0, 106.5, 109.1, 111.2, 113.7, 116.4,
	119.2, 121.9, 124.6, 127.6, 130.5, 133.0, 136.0, 139.6, 142.8, 145.8,
	149.1, 153.1, 156.6, 160.0, 164.1, 168.2, 173.3, 177.8, 182.5, 187.4,
	193.5, 199.4, 204.9, 211.6, 217.0, 222.9, 230.3, 239.0, 244.9, 251.4,
	259.3, 269.1, 277.1, 285.8, 294.0, 302.0, 306.9, 310.8, 318.0, 325.0,
	330.5, 336.3, 344.4, 352.9, 360.5, 369.0, 379.5, 389.2, 396.9, 407.7,
	418.3, 427.9, 436.1, 443.9, 452.3, 461.2, 469.9, 477.7, 487.1, 500.0,
	508.5, 516.3, 525.9, 535.4, 543.3, 552.1, 561.6, 572.4, 582.4, 592.7,
	605.1, 611.6, 618.9, 627.5, 634.9, 643.1, 650.1, 655.6, 660.9, 667.3,
	672.9, 679.4, 687.0, 696.2, 710.6, 721.1, 731.5, 741.3, 750.3, 760.1,
	768.8, 778.4, 786.2, 795.1, 804.7, 814.3, 825.1, 836.6, 848.4, 860.3,
	874.4, 888.0, 901.5, 914.7, 930.2, 942.3, 954.5, 966.7, 983.8, 998.8,
	1013.6, 1029.6, 1047.4, 1065.6, 1082.1, 1099.8, 1120.0, 1137.7,
	1152.9, 1167.7, 1181.5, 1191.4, 1204.9, 1216.4, 1229.2),
	order.by=seq(as.Date('1947-01-01'),as.Date('2004-01-01'),by='quarter'))

z <- as.numeric(quarter.PCE_housing_services['1959-01-01'] / quarter_PCE_housing_services_old['1959-01-01'])
t1 <- '1947-01-01'
t2 <- '1958-10-01'

### 2016-08-02 Paul Gomme

### The new housing services data includes utilities. Apparently, the data
### without utilities (which was used in earlier work) is no longer
### available. *sigh* To continue calculations, use the new data, and spline
### in the old data (since the new data does not go back far enough in time),
### adjusting the level to avoid a jump.

quarter_PCE_housing_services <- rbind(z*quarter_PCE_housing_services_old[paste(t1,t2,sep='/')],
                                      quarter.PCE_housing_services)

## The BLS only reports hours data since 1964.  Use some older data to get a
## longer time series.

## This is an old series I probably got from the old Citibase

quarter_hours_old_lhtpriva <- xts(c(80108, 79880, 80193, 81309, 81673, 81243,
	82021, 81400, 79611, 78052, 77174, 76173, 77072, 79762, 82855, 84277,
	85749, 86295, 85821, 85833, 86864, 86214, 86936, 89243, 90176, 90430,
	89652, 88286, 86536, 85866, 85337, 86301, 87745, 89619, 90551, 91622,
	92184, 92095, 91437, 92608, 92661, 92000, 91499, 89802, 87634, 85928,
	86902, 88467, 90456, 92320, 91723, 91875, 93041, 93032, 92387, 91273,
	90258, 90494, 91459, 92525, 92944, 94332, 94499, 94368, 94653, 95732,
	96132, 96539, 96782, 97884, 98538, 99635, 101022, 101947, 102782,
	104292, 105879, 106843, 107649, 108163, 107937, 107592, 108330,
	109161, 109403, 110414, 111566, 112320, 113478, 114545, 115420,
	115515, 115001, 113884, 113005, 111738, 112066, 112549, 112517,
	113745, 115032, 116436, 117146, 118849, 120585, 121858, 122443,
	123449, 123375, 123237, 123203, 121327, 117477, 116424, 117819,
	119459, 121425, 122071, 122486, 123206, 124525, 126746, 128152,
	129632, 130175, 133632, 134893, 136342, 137676, 138043, 138916,
	139358, 139343, 137028, 136357, 138308, 139617, 139457, 139627,
	138867, 137386, 135935, 134728, 133556, 133898, 135637, 137880,
	140694, 143067, 144765, 145949, 147134, 148057, 148791, 149421,
	150463, 151161, 151035, 151625, 152372, 153963, 155158, 156679,
	157774, 158735, 159951, 160972, 162455, 163564, 164037, 164288,
	164813, 166059, 166134, 165667, 164546, 163030, 162260, 162496,
	162423, 162064, 163165, 163324, 164365, 165436, 166653, 168011,
	169496, 170641, 173060, 174580, 176175, 177057, 177279, 178242,
	178998, 179596, 181458, 182971, 184464, 185961, 187584, 188985,
	190758, 192261, 192946, 194065, 195379, 196072, 197139, 198386,
	199815, 201113, 201418, 201618, 201787, 201746, 200987, 199697,
	197803, 197486, 197433, 197034),
	order.by=seq(as.Date('1947-01-01'),as.Date('2002-07-01'),by='quarter'))

z <- as.numeric(quarter.hours_private_nonfarm['1964-01-01'] / quarter_hours_old_lhtpriva['1964-01-01'])
t1 <- '1947-01-01'
t2 <- '1963-10-01'

quarter_hours <- rbind(z*quarter_hours_old_lhtpriva[paste(t1,t2,sep='/')], quarter.hours_private_nonfarm)

### Nominal private inventories, from Haver.

quarter_private_inventories_haver <- xts(c(93.0, 94.2, 99.5, 107.0, 103.5,
	105.1, 104.3, 102.3, 100.2, 95.5, 94.8, 92.5, 94.1, 97.3, 103.7,
	114.4, 122.7, 123.7, 124.8, 127.3, 125.9, 124.1, 122.8, 118.9, 117.4,
	116.7, 117.0, 117.0, 117.3, 115.8, 115.6, 115.0, 116.5, 115.8, 115.9,
	115.9, 118.8, 122.7, 123.1, 124.7, 125.6, 126.8, 128.1, 127.8, 130.9,
	129.8, 131.2, 131.5, 132.8, 134.1, 133.4, 133.1, 136.8, 136.0, 137.4,
	136.4, 135.9, 134.8, 137.7, 139.8, 142.7, 143.6, 147.1, 147.4, 147.9,
	148.5, 149.3, 149.9, 150.4, 150.7, 153.0, 154.5, 158.9, 163.1, 164.6,
	169.4, 174.8, 179.1, 183.5, 185.6, 187.6, 190.1, 192.0, 194.8, 199.2,
	203.4, 205.7, 208.1, 213.7, 218.9, 222.5, 227.4, 230.3, 232.9, 235.6,
	235.7, 243.2, 247.3, 251.2, 253.7, 258.6, 266.7, 274.2, 283.6, 304.2,
	324.1, 338.1, 351.5, 361.5, 372.7, 396.8, 405.6, 400.5, 404.4, 409.4,
	408.5, 415.9, 428.2, 433.6, 439.6, 451.4, 458.3, 466.5, 482.0, 507.9,
	530.0, 547.6, 570.9, 609.3, 629.2, 650.8, 667.6, 691.5, 708.4, 727.2,
	739.0, 763.8, 773.5, 780.9, 779.1, 788.2, 788.6, 783.2, 773.9, 772.4,
	775.3, 782.5, 796.9, 827.4, 844.9, 856.7, 869.0, 865.7, 861.3, 858.1,
	872.6, 862.0, 859.3, 856.8, 854.9, 872.4, 888.8, 898.1, 922.4, 941.2,
	962.3, 983.9, 1000.9, 1025.7, 1033.2, 1033.9, 1045.0, 1051.6, 1057.8,
	1078.8, 1082.3, 1070.0, 1054.6, 1051.0, 1058.5, 1064.8, 1075.9,
	1085.9, 1082.6, 1090.8, 1100.9, 1104.1, 1116.0, 1133.1, 1147.4,
	1164.6, 1194.5, 1224.9, 1241.4, 1247.6, 1257.2, 1257.8, 1269.1,
	1283.5, 1284.7, 1293.9, 1305.1, 1315.1, 1327.9, 1338.0, 1336.7,
	1334.6, 1342.3, 1360.1, 1374.0, 1397.6, 1433.4, 1457.5, 1480.6,
	1496.8, 1524.8, 1523.1, 1509.9, 1485.2, 1447.7, 1448.8, 1456.5,
	1475.0, 1495.2, 1518.2, 1512.6, 1530.2, 1556.6, 1594.4, 1636.6,
	1665.2, 1697.9, 1745.0, 1750.7, 1792.1, 1842.3, 1857.2, 1900.4,
	1934.1, 1953.4, 1993.8, 2022.9, 2050.8, 2119.0, 2194.5, 2285.4,
	2251.8, 2050.1, 1974.0, 1933.4, 1899.0, 1927.2, 1975.2, 1979.3,
	2042.1, 2129.5, 2229.6, 2274.0, 2279.3, 2300.9, 2339.0, 2329.8,
	2381.9, 2392.0, 2405.7, 2402.5, 2419.4, 2443.9, 2488.8, 2516.9,
	2517.5, 2496.0, 2467.9),
	order.by=seq(as.Date('1947-01-01'),as.Date('2015-01-01'),by='quarter'))

t1 = '1947-01-01'
t2 = '1996-07-01'
quarter_private_inventories <- rbind(quarter_private_inventories_haver[paste(t1,t2,sep='/')], quarter.private_inventories)

### End of splicing

quarter_real_inventories <- quarter_private_inventories / (lag(quarter_deflator_consumption,-1)/100)

### `Usual' macroaggregates

quarter_real_y = (quarter.GDP  - quarter_PCE_housing_services)/ 
    (quarter_deflator_consumption/100) 
quarter_real_yp = quarter_real_y - quarter.gov_wages_and_salaries / 
    (quarter_deflator_consumption/100) 
quarter_real_xs = 
    quarter.private_investment_nonresidential_structures / 
    (quarter_deflator_consumption/100) 
quarter_real_xe = quarter.private_investment_equipment_software / 
    (quarter_deflator_consumption/100) 
quarter_real_xh = quarter.private_investment_residential_structures / 
    (quarter_deflator_consumption/100) 
quarter_real_xd = quarter.PCE_durables / (quarter_deflator_consumption/100)
quarter_real_cm = (quarter.PCE_nondurables + quarter.PCE_services - 
    quarter_PCE_housing_services) / (quarter_deflator_consumption/100) 

### Impute to quarterly. In the earlier spreadsheets and Matlab code, the
### annual observation was simply repeated. Here, I use R's spline
### interpolation instead.

dt.start <- start(annual.housing_net_operating_surplus)
dt.end <- end(annual.housing_net_operating_surplus)
yr.start <- as.numeric(format(dt.start, "%Y"))
yr.end <- as.numeric(format(dt.end, "%Y"))

yr.a <- seq(yr.start,yr.end)
yr.q <- seq(yr.start,yr.end,1/4)
t1 <- as.Date(sprintf('%i-01-01', yr.start))
t2 <- as.Date(sprintf('%i-01-01', yr.end))

work.spl <- smooth.spline(yr.a, annual.housing_net_operating_surplus / annual.gross_housing_value_added)
work.sp <- predict(work.spl, yr.q)
work.xts <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))
quarter_housing_net_operating_surplus = quarter.gross_housing_value_added * work.xts

work.spl <- smooth.spline(yr.a, annual.housing_net_interest / annual.gross_housing_value_added)
work.sp <- predict(work.spl, yr.q)
work.xts <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))
quarter_housing_net_interest = quarter.gross_housing_value_added * work.xts

work.spl <- smooth.spline(yr.a, annual.housing_proprietors_income / annual.gross_housing_value_added)
work.sp <- predict(work.spl, yr.q)
work.xts <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))
quarter_housing_proprietors_income = quarter.gross_housing_value_added * work.xts

work.spl <- smooth.spline(yr.a, annual.housing_rental_income / annual.gross_housing_value_added)
work.sp <- predict(work.spl, yr.q)
work.xts <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))
quarter_housing_rental_income = quarter.gross_housing_value_added * work.xts

work.spl <- smooth.spline(yr.a, annual.housing_corp_profits / annual.gross_housing_value_added)
work.sp <- predict(work.spl, yr.q)
work.xts <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))
quarter_housing_corp_profits = quarter.gross_housing_value_added * work.xts

work.spl <- smooth.spline(yr.a, annual.state_local_property_taxes)
work.sp <- predict(work.spl, yr.q)
quarter_state_local_property_taxes_spline <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))
t1 <- as.Date('1947-01-01')
t2 <- as.Date('1987-10-01')
quarter_property_taxes_household <- rbind(quarter_state_local_property_taxes_spline[paste(t1,t2,sep='/')], quarter.state_local_property_taxes/1000)

dt.start <- start(annual.state_local_gov_property_taxes)
dt.end <- end(annual.state_local_gov_property_taxes)
yr.start <- as.numeric(format(dt.start, "%Y"))
yr.end <- as.numeric(format(dt.end, "%Y"))

yr.a <- seq(yr.start,yr.end)
yr.q <- seq(yr.start,yr.end,1/4)
t1 <- as.Date(sprintf('%i-01-01', yr.start))
t2 <- as.Date(sprintf('%i-01-01', yr.end))

work.spl <- smooth.spline(yr.a, annual.state_local_gov_property_taxes)
work.sp <- predict(work.spl, yr.q)
###t1 <- as.Date('1929-01-01')
###t2 <- as.Date('2015-01-01')
quarter_state_local_gov_property_taxes_spline <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))
t1 <- as.Date('1947-01-01')
t2 <- as.Date('1957-10-01')
quarter_real_estate_property_taxes <- rbind(quarter_state_local_gov_property_taxes_spline[paste(t1,t2,sep='/')], quarter.state_local_gov_property_taxes)

### 2019-07-03: See discussion below in calculation of quarter_tau_k

###dt.start <- start(annual.state_local_gov_other_taxes)
###dt.end <- end(annual.state_local_gov_other_taxes)
###yr.start <- as.numeric(format(dt.start, "%Y"))
###yr.end <- as.numeric(format(dt.end, "%Y"))
###
###yr.a <- seq(yr.start,yr.end)
###yr.q <- seq(yr.start,yr.end,1/4)
###t1 <- as.Date(sprintf('%i-01-01', yr.start))
###t2 <- as.Date(sprintf('%i-01-01', yr.end))
###
###work.spl <- smooth.spline(yr.a, annual.state_local_gov_other_taxes)
###work.sp <- predict(work.spl, yr.q)
######t1 <- as.Date('1929-01-01')
######t2 <- as.Date('2014-01-01')
###quarter_other_taxes <- xts(work.sp$y, order.by=seq(t1, t2, by='quarter'))

#############################################################################
find.delta <- function(delta.in)
  {
    y <- (1-delta.in[1])^4*k.g + (1-delta.in[1])^3*x.g[1] + 
      (1-delta.in[1])^2*x.g[2] + (1-delta.in[1])*x.g[3] + x.g[4] - kprime.g
    y
  }
#############################################################################

### The following code uses annual capital stock data and quarterly
### investment data to construct quarterly capital stocks under the
### assumption that depreciation within the year. The function find.delta
### finds such a depreciation rate.

### Note: Capital stock data is reported as of the end of the year.

t1 <- as.Date('1947-01-01')
t2 <- as.Date(end(annual_real_ke))
t3 <- as.Date('2016-01-01')

i.start <- 18
i.end <- length(as.numeric(annual_real_ke))-1

### Equipment (and software)

annual_delta_e <- rep(0, (i.end-i.start+1))
quarter_real_ke <- rep(0, 4*(i.end-i.start+1))

for (iyear in i.start:i.end)
  {
    iquarter <- (iyear-i.start)*4
    k.g <- as.numeric(annual_real_ke[iyear])
    kprime.g <- as.numeric(annual_real_ke[iyear+1])
    x.g <- as.numeric(quarter_real_xe[(iquarter+1):(iquarter+4)])
    delta.g <- 0.1
    ans <- nleqslv(delta.g, find.delta, control=list(ftol=1e-12,xtol=1e-16))
    annual_delta_e[iyear-i.start+1] <- ans$x
    if (iyear == i.start) quarter_real_ke[iquarter+1] <- annual_real_ke[iyear]
    for (jquarter in 1:4)
      {
        quarter_real_ke[iquarter+1+jquarter] <- (1-annual_delta_e[iyear-i.start+1])*quarter_real_ke[iquarter+jquarter] + quarter_real_xe[iquarter+jquarter]
      }
  }

###annual_delta_e <- xts(annual_delta_e, order.by=seq(t1, t2, by='year'))
###quarter_real_ke <- xts(quarter_real_ke, order.by=seq(t1, t3, by='quarter'))
### 2017-09-06: Fix end date problem.
annual_delta_e <- as.xts(ts(annual_delta_e, start=1947, frequency=1))
quarter_real_ke <- as.xts(ts(quarter_real_ke, start=1947, frequency=4))

### Nonresidential structures

annual_delta_s <- rep(0, (i.end-i.start+1))
quarter_real_ks <- rep(0, 4*(i.end-i.start+1))

for (iyear in i.start:i.end)
  {
    iquarter <- (iyear-i.start)*4
    k.g <- as.numeric(annual_real_ks[iyear])
    kprime.g <- as.numeric(annual_real_ks[iyear+1])
    x.g <- as.numeric(quarter_real_xs[(iquarter+1):(iquarter+4)])
    delta.g <- 0.1
    ans <- nleqslv(delta.g, find.delta, control=list(ftol=1e-12,xtol=1e-16))
    annual_delta_s[iyear-i.start+1] <- ans$x
    if (iyear == i.start) quarter_real_ks[iquarter+1] <- annual_real_ks[iyear]
    for (jquarter in 1:4)
      {
        quarter_real_ks[iquarter+1+jquarter] <- (1-annual_delta_s[iyear-i.start+1])*quarter_real_ks[iquarter+jquarter] + quarter_real_xs[iquarter+jquarter]
      }
  }

###annual_delta_s <- xts(annual_delta_s, order.by=seq(t1, t2, by='year'))
###quarter_real_ks <- xts(quarter_real_ks, order.by=seq(t1, t3, by='quarter'))
### 2017-09-06: Fix end date problem.
annual_delta_s <- as.xts(ts(annual_delta_s, start=1947, frequency=1))
quarter_real_ks <- as.xts(ts(quarter_real_ks, start=1947, frequency=4))

### Residential structures (housing)

annual_delta_h <- rep(0, (i.end-i.start+1))
quarter_real_kh <- rep(0, 4*(i.end-i.start+1))

for (iyear in i.start:i.end)
  {
    iquarter <- (iyear-i.start)*4
    k.g <- as.numeric(annual_real_kh[iyear])
    kprime.g <- as.numeric(annual_real_kh[iyear+1])
    x.g <- as.numeric(quarter_real_xh[(iquarter+1):(iquarter+4)])
    delta.g <- 0.1
    ans <- nleqslv(delta.g, find.delta, control=list(ftol=1e-12,xtol=1e-16))
    annual_delta_h[iyear-i.start+1] <- ans$x
    if (iyear == i.start) quarter_real_kh[iquarter+1] <- annual_real_kh[iyear]
    for (jquarter in 1:4)
      {
        quarter_real_kh[iquarter+1+jquarter] <- (1-annual_delta_h[iyear-i.start+1])*quarter_real_kh[iquarter+jquarter] + quarter_real_xh[iquarter+jquarter]
      }
  }

###annual_delta_h <- xts(annual_delta_h, order.by=seq(t1, t2, by='year'))
###quarter_real_kh <- xts(quarter_real_kh, order.by=seq(t1, t3, by='quarter'))
### 2017-09-06: Fix end date problem.
annual_delta_h <- as.xts(ts(annual_delta_h, start=1947, frequency=1))
quarter_real_kh <- as.xts(ts(quarter_real_kh, start=1947, frequency=4))

### Consumer durables

annual_delta_d <- rep(0, (i.end-i.start+1))
quarter_real_kd <- rep(0, 4*(i.end-i.start+1))

for (iyear in i.start:i.end)
  {
    iquarter <- (iyear-i.start)*4
    k.g <- as.numeric(annual_real_kd[iyear])
    kprime.g <- as.numeric(annual_real_kd[iyear+1])
    x.g <- as.numeric(quarter_real_xd[(iquarter+1):(iquarter+4)])
    delta.g <- 0.1
    ans <- nleqslv(delta.g, find.delta, control=list(ftol=1e-12,xtol=1e-16))
    annual_delta_d[iyear-i.start+1] <- ans$x
    if (iyear == i.start) quarter_real_kd[iquarter+1] <- annual_real_kd[iyear]
    for (jquarter in 1:4)
      {
        quarter_real_kd[iquarter+1+jquarter] <- (1-annual_delta_d[iyear-i.start+1])*quarter_real_kd[iquarter+jquarter] + quarter_real_xd[iquarter+jquarter]
      }
  }

###annual_delta_d <- xts(annual_delta_d, order.by=seq(t1, t2, by='year'))
###quarter_real_kd <- xts(quarter_real_kd, order.by=seq(t1, t3, by='quarter'))
### 2017-09-06: Fix end date problem.
annual_delta_d <- as.xts(ts(annual_delta_d, start=1947, frequency=1))
quarter_real_kd <- as.xts(ts(quarter_real_kd, start=1947, frequency=4))

################################################################################

### Calculations of tax rates as in Mendoza, Razin and Tesar

quarter_real_estate_taxes_business <- 
    quarter_real_estate_property_taxes * quarter_real_ks / 
    (quarter_real_ks + quarter_real_kh)
quarter_real_estate_taxes_household <- 
    quarter_real_estate_property_taxes * quarter_real_kh / 
    (quarter_real_ks + quarter_real_kh)

quarter_business_income_pre_tax <- 
    quarter.net_operating_surplus_private - 
    quarter_housing_net_operating_surplus - 
    (1-alpha_mean)*(quarter.proprietors_income - 
	       quarter_housing_proprietors_income) 

quarter_tau_h <- quarter.personal_taxes / 
    (quarter.wage_salary_accruals + quarter.net_interest + 
     quarter.proprietors_income + quarter.rental_income)

quarter_tau_n <- (quarter_tau_h*(quarter.wage_salary_accruals + 
				(1-alpha_mean)*quarter.proprietors_income) 
		 + quarter.contributions_gov_social_insurance) / 
    (quarter.wage_salary_accruals + 
     (1-alpha_mean)*quarter.proprietors_income + 
     quarter.employer_contributions_gov_social_insurance) 

### 2019-07-03: The series used to obtain quarter_other_taxes has been
### discontinued by the BEA. Digging into that data series revealed a number of
### conceptual issues such as:
###   (a) Prior to 1931, included excise taxes on alcohol and tobacco (conceptually sales taxes)
###   (b) Prior to 1958, included a number of excise taxes (public utilities, insurance receipts, other excise taxes)
###   (c) Prior to 1959, included severance taxes (for removing natural resources, chiefly oil)
### The changes in the late 1950s lead to a noticeable drop in this "other taxes" series.
### In summary, this "other taxes" series has long included taxes that more properly
### are categorized excise taxes, and so should be part of consumption taxes.
### Lacking firm knowledge of what exactly is included in "other taxes", I'm simply
### droping it from the calculation of the capital income tax rate.

###quarter_tau_k <- (quarter_tau_h*(quarter.net_interest + 
###				 alpha_mean*quarter.proprietors_income + 
###				 quarter.rental_income - 
###				 (quarter_housing_net_interest + 
###				  alpha_mean*quarter_housing_proprietors_income 
###				  + quarter_housing_rental_income)) + 
###		 quarter.gov_taxes_corp_income + 
###		 quarter_real_estate_taxes_business + quarter_other_taxes) / 
###    (quarter.net_operating_surplus_private 
###      - quarter_housing_net_operating_surplus - 
###     (1-alpha_mean)*(quarter.proprietors_income - 
###		quarter_housing_proprietors_income))  

quarter_tau_k <- (quarter_tau_h*(quarter.net_interest + 
				 alpha_mean*quarter.proprietors_income + 
				 quarter.rental_income - 
				 (quarter_housing_net_interest + 
				  alpha_mean*quarter_housing_proprietors_income 
				  + quarter_housing_rental_income)) + 
		 quarter.gov_taxes_corp_income + 
		 quarter_real_estate_taxes_business) / 
    (quarter.net_operating_surplus_private 
      - quarter_housing_net_operating_surplus - 
     (1-alpha_mean)*(quarter.proprietors_income - 
		quarter_housing_proprietors_income))  

### 2019-09-10: The following capital tax rate calculation is consistent with
### the measurement in Gomme and Rupertr (2007)

quarter_tau_k_gr <- (quarter_tau_h*(quarter.net_interest + 
				 alpha_mean*quarter.proprietors_income + 
				 quarter.rental_income - 
				 (quarter_housing_net_interest + 
				  alpha_mean*quarter_housing_proprietors_income 
				  + quarter_housing_rental_income)) + 
		 quarter.gov_taxes_corp_income + 
		 quarter_real_estate_taxes_business) / 
    (quarter.net_operating_surplus_private 
      - quarter_housing_net_operating_surplus
      + quarter.private_capital_consumption
      - quarter.private_capital_consumption_housing
      - (1-alpha_mean)*(quarter.proprietors_income - 
		quarter_housing_proprietors_income))  

### 2016-08-15: Added calculation for consumption tax rate

quarter_consumption_taxes <- quarter.state_local_sales_taxes + quarter.gov_excise_taxes + quarter.gov_custom_duties

quarter_tau_c <- quarter_consumption_taxes /
  (quarter.PCE_nondurables
   + quarter.PCE_services
   + quarter.PCE_durables
   + quarter.gov_consumption_expenditures
   - quarter_consumption_taxes)

annual_consumption_taxes <- annual.state_local_sales_taxes + annual.gov_excise_taxes + annual.gov_custom_duties
annual_tau_c <- annual_consumption_taxes /
  (annual.PCE_nondurables
   + annual.PCE_services
   + annual.PCE_durables
   + annual.gov_consumption_expenditures
   - annual_consumption_taxes)

tau_k = mean(quarter_tau_k)
tau_n = mean(quarter_tau_n)

### Return to capital series need tax rates for after-tax calculations.

### 2019-07-03: See discussion of "other taxes" in calculation of quarter_tau_k

###quarter_business_income_after_tax = quarter_business_income_pre_tax - 
###    (quarter_tau_h*(quarter.net_interest + 
###		    alpha_mean*quarter.proprietors_income + 
###		    quarter.rental_income - 
###		    (quarter_housing_net_interest + 
###		     alpha_mean*quarter_housing_proprietors_income + 
###		     quarter_housing_rental_income)) + 
###     quarter.gov_taxes_corp_income + quarter_other_taxes + 
###     quarter_real_estate_taxes_business)
###
###quarter_all_capital_income_pre_tax = 
###    quarter.net_operating_surplus_private - 
###    (1-alpha_mean)*quarter.proprietors_income 
###quarter_all_capital_income_after_tax = 
###    quarter_all_capital_income_pre_tax - 
###    (quarter_tau_h*(quarter.net_interest +  
###		    alpha_mean*quarter.proprietors_income + 
###		    quarter.rental_income) + 
###     quarter.gov_taxes_corp_income + quarter_other_taxes + 
###     quarter_real_estate_taxes_business + 
###     quarter_property_taxes_household) 

quarter_business_income_after_tax = quarter_business_income_pre_tax - 
    (quarter_tau_h*(quarter.net_interest + 
		    alpha_mean*quarter.proprietors_income + 
		    quarter.rental_income - 
		    (quarter_housing_net_interest + 
		     alpha_mean*quarter_housing_proprietors_income + 
		     quarter_housing_rental_income)) + 
     quarter.gov_taxes_corp_income + 
     quarter_real_estate_taxes_business)

quarter_all_capital_income_pre_tax = 
    quarter.net_operating_surplus_private - 
    (1-alpha_mean)*quarter.proprietors_income 
quarter_all_capital_income_after_tax = 
    quarter_all_capital_income_pre_tax - 
    (quarter_tau_h*(quarter.net_interest +  
		    alpha_mean*quarter.proprietors_income + 
		    quarter.rental_income) + 
     quarter.gov_taxes_corp_income + 
     quarter_real_estate_taxes_business + 
     quarter_property_taxes_household) 

### The `usual' macroaggregates, this time expressed per capita.

quarter_real_pc_y = 1000*quarter_real_y / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_yp = 1000*quarter_real_yp / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_cm = 1000*quarter_real_cm / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_xs = 1000*quarter_real_xs / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_xe = 1000*quarter_real_xe / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_xh = 1000*quarter_real_xh / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_xd = 1000*quarter_real_xd / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_ks = 1000*quarter_real_ks / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_ke = 1000*quarter_real_ke / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_kh = 1000*quarter_real_kh / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_real_pc_kd = 1000*quarter_real_kd / as.xts(quarter.civilian_noninstitutional_pop_16_plus)
quarter_pc_hours = 1000*quarter_hours / as.xts(quarter.civilian_noninstitutional_pop_16_plus)

quarter_capital_gain = quarter_relative_price_investment /
  lag(quarter_relative_price_investment)
mean_capital_gain = mean(quarter_capital_gain)

### Each return to capital series is computed pre- and after-tax. In the
### `basic' calculation, a `capital gain' is included in the return. This
### capital gain is the change in the relative price of investment goods (not
### from stock market data, or price of housing data). `no_gain' versions of
### the returns simply omit this capital gain component. The `constant_gain'
### versions use the mean capital gain in the place of the time-varying
### capital gain.

### Returns to business capital

quarter_return_business_capital_pre_tax =
  100*((quarter_business_income_pre_tax/4 /
        (quarter_deflator_consumption/100) /
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke))
        + quarter_capital_gain)^4 - 1)

quarter_return_business_capital_after_tax = 
  100*((quarter_business_income_after_tax/4 / 
        (quarter_deflator_consumption/100) / 
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke))  
        + quarter_capital_gain)^4 - 1) 

quarter_return_business_capital_pre_tax_no_gain =
  100*((quarter_business_income_pre_tax/4 /
        (quarter_deflator_consumption/100) /
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke)) + 1)^4
        - 1)

quarter_return_business_capital_after_tax_no_gain = 
  100*((quarter_business_income_after_tax/4 / 
        (quarter_deflator_consumption/100) / 
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke)) + 1)^4 - 1) 

quarter_return_business_capital_pre_tax_constant_gain =
  100*((quarter_business_income_pre_tax/4 /
        (quarter_deflator_consumption/100) /
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke)) + mean_capital_gain)^4
        - 1)

quarter_return_business_capital_after_tax_constant_gain = 
  100*((quarter_business_income_after_tax/4 / 
        (quarter_deflator_consumption/100) / 
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke)) + mean_capital_gain)^4 - 1) 

### Returns to all capital income (business and household)

quarter_return_all_capital_pre_tax =
  100*((quarter_all_capital_income_pre_tax/4 /
        (quarter_deflator_consumption/100) /
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke) + lag(quarter_real_kh))
        + quarter_capital_gain)^4 - 1)

quarter_return_all_capital_after_tax = 
  100*((quarter_all_capital_income_after_tax/4 / 
        (quarter_deflator_consumption/100) / 
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke) + lag(quarter_real_kh))
        + quarter_capital_gain)^4 - 1) 

quarter_return_all_capital_pre_tax_no_gain =
  100*((quarter_all_capital_income_pre_tax/4 /
        (quarter_deflator_consumption/100) /
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke) + lag(quarter_real_kh)) + 1)^4
        - 1)

quarter_return_all_capital_after_tax_no_gain = 
  100*((quarter_all_capital_income_after_tax/4 / 
        (quarter_deflator_consumption/100) / 
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke) + lag(quarter_real_kh)) + 1)^4 - 1) 

quarter_return_all_capital_pre_tax_constant_gain =
  100*((quarter_all_capital_income_pre_tax/4 /
        (quarter_deflator_consumption/100) /
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke) + lag(quarter_real_kh)) + mean_capital_gain)^4
        - 1)

quarter_return_all_capital_after_tax_constant_gain = 
  100*((quarter_all_capital_income_after_tax/4 / 
        (quarter_deflator_consumption/100) / 
        (lag(quarter_real_inventories)
         + lag(quarter_real_ks)
         + lag(quarter_real_ke) + lag(quarter_real_kh)) + mean_capital_gain)^4 - 1) 

### Returns to housing income

quarter_return_housing_capital_pre_tax =
  100*(((quarter_all_capital_income_pre_tax-quarter_business_income_pre_tax)/4 /
        (quarter_deflator_consumption/100) /
        lag(quarter_real_kh) + quarter_capital_gain)^4 - 1)

quarter_return_housing_capital_after_tax = 
  100*(((quarter_all_capital_income_after_tax-quarter_business_income_after_tax)/4 / 
        (quarter_deflator_consumption/100) / lag(quarter_real_kh) + quarter_capital_gain)^4 - 1) 

quarter_return_housing_capital_pre_tax_no_gain =
  100*(((quarter_all_capital_income_pre_tax-quarter_business_income_pre_tax)/4 /
        (quarter_deflator_consumption/100) / lag(quarter_real_kh) + 1)^4 - 1)

quarter_return_housing_capital_after_tax_no_gain = 
  100*(((quarter_all_capital_income_after_tax-quarter_business_income_after_tax)/4 / 
        (quarter_deflator_consumption/100) / lag(quarter_real_kh) + 1)^4 - 1) 

quarter_return_housing_capital_pre_tax_constant_gain =
  100*(((quarter_all_capital_income_pre_tax-quarter_business_income_pre_tax)/4 /
        (quarter_deflator_consumption/100) / lag(quarter_real_kh) + mean_capital_gain)^4 - 1)

quarter_return_housing_capital_after_tax_constant_gain = 
  100*(((quarter_all_capital_income_after_tax-quarter_business_income_after_tax)/4 / 
        (quarter_deflator_consumption/100) / lag(quarter_real_kh) + mean_capital_gain)^4 - 1) 

### The Solow residual (total factor productivity)

quarter_solow_residual <- (quarter_real_y / (lag(quarter_real_ke)+lag(quarter_real_ks))^alpha_mean)^(1/(1-alpha_mean)) / quarter_hours

### Aggregated investment series

quarter_real_pc_xall <- quarter_real_pc_xs+quarter_real_pc_xe + quarter_real_pc_xh+quarter_real_pc_xd
quarter_real_pc_xmarket <- quarter_real_pc_xs+quarter_real_pc_xe
quarter_real_pc_xhome <- quarter_real_pc_xh+quarter_real_pc_xd
quarter_real_pc_kall <- quarter_real_pc_ks+quarter_real_pc_ke + quarter_real_pc_kh+quarter_real_pc_kd
quarter_real_pc_kmarket <- quarter_real_pc_ks+quarter_real_pc_ke
quarter_real_pc_khome <- quarter_real_pc_kh+quarter_real_pc_kd
quarter_productivity <- quarter_real_pc_y/quarter_pc_hours

### Collection of data that is Hodrick-Prescott filtered

print(" Preparing to HP filter ")


t.start <- '1954-01-01'
#t.end <- '2016-04-01'

###us.data.1 <- merge(quarter_real_pc_y[paste(t.start,t.end,sep='/')], 
###	   quarter_real_pc_cm[paste(t.start,t.end,sep='/')], 
###	   quarter_real_pc_xall[paste(t.start,t.end,sep='/')],  
###	   quarter_real_pc_xmarket[paste(t.start,t.end,sep='/')], 
###	   quarter_real_pc_xhome[paste(t.start,t.end,sep='/')],  
###	   quarter_pc_hours[paste(t.start,t.end,sep='/')], 
###	   quarter_productivity[paste(t.start,t.end,sep='/')], 
###	   quarter_real_pc_kall[paste(t.start,t.end,sep='/')], 
###	   quarter_real_pc_kmarket[paste(t.start,t.end,sep='/')], 
###	   quarter_real_pc_kall[paste(t.start,t.end,sep='/')], 
###	   quarter_solow_residual[paste(t.start,t.end,sep='/')], 
###	   quarter_relative_price_investment[paste(t.start,t.end,sep='/')], 
###	   quarter_tau_k[paste(t.start,t.end,sep='/')], 
###	   quarter_tau_n[paste(t.start,t.end,sep='/')])

us.data.1 <- merge(quarter_real_pc_y, 
	   quarter_real_pc_cm, 
	   quarter_real_pc_xall,  
	   quarter_real_pc_xmarket, 
	   quarter_real_pc_xhome,  
	   quarter_pc_hours, 
	   quarter_productivity, 
	   quarter_real_pc_kall, 
	   quarter_real_pc_kmarket, 
	   quarter_real_pc_khome, 
	   quarter_solow_residual, 
	   quarter_relative_price_investment, 
	   quarter_tau_k, 
	   quarter_tau_n)

### Collection of data that is expressed as a percentage deviation from its mean

###us.data.2 <- merge(quarter_return_business_capital_pre_tax[paste(t.start,t.end,sep='/')], 
###	   quarter_return_business_capital_pre_tax_no_gain[paste(t.start,t.end,sep='/')], 
###	   quarter_return_business_capital_after_tax[paste(t.start,t.end,sep='/')], 
###	   quarter_return_business_capital_after_tax_no_gain[paste(t.start,t.end,sep='/')], 
###	   quarter_return_all_capital_pre_tax[paste(t.start,t.end,sep='/')], 
###	   quarter_return_all_capital_pre_tax_no_gain[paste(t.start,t.end,sep='/')], 
###	   quarter_return_all_capital_after_tax[paste(t.start,t.end,sep='/')], 
###	   quarter_return_all_capital_after_tax_no_gain[paste(t.start,t.end,sep='/')], 
###	   quarter_return_housing_capital_pre_tax[paste(t.start,t.end,sep='/')], 
###	   quarter_return_housing_capital_pre_tax_no_gain[paste(t.start,t.end,sep='/')], 
###	   quarter_return_housing_capital_after_tax[paste(t.start,t.end,sep='/')], 
###	   quarter_return_housing_capital_after_tax_no_gain[paste(t.start,t.end,sep='/')])

us.data.2 <- merge(quarter_return_business_capital_pre_tax, 
	   quarter_return_business_capital_pre_tax_no_gain, 
	   quarter_return_business_capital_after_tax, 
	   quarter_return_business_capital_after_tax_no_gain, 
	   quarter_return_all_capital_pre_tax, 
	   quarter_return_all_capital_pre_tax_no_gain, 
	   quarter_return_all_capital_after_tax, 
	   quarter_return_all_capital_after_tax_no_gain, 
	   quarter_return_housing_capital_pre_tax, 
	   quarter_return_housing_capital_pre_tax_no_gain, 
	   quarter_return_housing_capital_after_tax, 
	   quarter_return_housing_capital_after_tax_no_gain)

xxx <- merge(us.data.1, us.data.2)
xxx <- xxx[complete.cases(xxx)]
t.end <- end(xxx)

us.data.1 <- us.data.1[paste(t.start,t.end,sep='/'),]
us.data.2 <- us.data.2[paste(t.start,t.end,sep='/'),]

us.data.filt <- xts(, order.by=seq(as.Date(t.start),as.Date(t.end),by='quarter'))

for (i in 1:dim(us.data.1)[2])
  {
    y <- hpfilter(log(us.data.1[,i]), type="lambda", freq=1600)
    us.data.filt <- merge(us.data.filt, xts(y$cycle, order.by=seq(as.Date(t.start),as.Date(t.end),by='quarter')))
  }

for (i in 1:dim(us.data.2)[2])
  {
    y.mean <- mean(us.data.2[,i])
    us.data.filt <- merge(us.data.filt, (us.data.2[,i] - y.mean) / y.mean)
  }

T <- dim(us.data.filt)
N = T[2]
T = T[1]

print(" Done HP filtering ")


### Produce a table of second moments

tex.file <- 'us-moments.tex'

us.sstrg = c('Output', 'Consumption', 'Investment', 'Investment: market', 
	 'Investment: home', 'Hours', 
         'Productivity', 'Capital',  'Capital: market', 'Capital: home', 
	 'Solow Residual', 
	 'Price of Investment', 'Capital tax', 'Labor tax', 
	 'Business capital, pre-tax, capital gain', 
	 'Business capital, pre-tax, no capital gain', 
	 'Business capital, after-tax, capital gain', 
	 'Business capital, after-tax, no capital gain', 
	 'All capital, pre-tax, capital gain', 
	 'All capital, pre-tax, no capital gain', 
	 'All capital, after-tax, capital gain', 
	 'All capital, after-tax, no capital gain', 
	 'Housing capital, pre-tax, capital gain', 
	 'Housing capital, pre-tax, no capital gain', 
	 'Housing capital, after-tax, capital gain', 
	 'Housing capital, after-tax, no capital gain')


if (file.exists(tex.file))
  {
    file.remove(tex.file)
  }

cat(sprintf('\\documentclass[12pt]{article}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\usepackage{graphicx,dcolumn,rotating,multirow,times,mathptmx,booktabs}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\usepackage[margin=1in]{geometry}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\newcolumntype{.}{D{.}{.}{-1}}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\newcolumntype{d}[1]{D{.}{.}{#1}}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\begin{document}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\begin{sidewaystable}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\begin{center}\n'), file=tex.file, append=TRUE)
###cat(sprintf('\\caption{U.S.\\@ 1954Q1--2015Q1: Selected Moments}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\caption{U.S.\\@ %s--%s: Selected Moments}\n', t.start, t.end), file=tex.file, append=TRUE)
cat(sprintf('\\label{tab:rbc-lead-lag}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\begin{tabular}{l . . . . . . . . . .}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\toprule\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c }{\\multirow{2}*{\\parbox[t]{1in}{\\centering Standard Deviation}}}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{9}{c}{Cross Correlation of Real Output With}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\\\ \\cmidrule{3-11} \n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t&\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t-4}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t-3}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t-2}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t-1}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_t$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t+1}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t+2}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t+3}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\t\t& \\multicolumn{1}{c}{$x_{t+4}$}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\\\\n'), file=tex.file, append=TRUE)
cat(sprintf('\\midrule\n'), file=tex.file, append=TRUE)

for (i in 1:N)
  {
    us.sd <- 100*sqrt(var(as.numeric(us.data.filt[,i])))
    us.ac <- cor(us.data.filt[1:(T-1),i], us.data.filt[2:T,i])
    zzz <- ccf(as.ts(us.data.filt[,1]), as.ts(us.data.filt[,i]), lag.max =4)
    us.ccf <- as.numeric(zzz$acf)
    ##print(sprintf("%f %f %f %f %f %f %f %f %f %f", us.sd, us.ccf[1], us.ccf[2], us.ccf[3], us.ccf[4], us.ccf[5], us.ccf[6], us.ccf[7], us.ccf[8], us.ccf[9]))
    cat(sprintf("%-50s & %8.2f & %8.2f & %8.2f & %8.2f & %8.2f & %8.2f & %8.2f & %8.2f & %8.2f & %8.2f\\\\", us.sstrg[i], us.sd, us.ccf[9], us.ccf[8], us.ccf[7], us.ccf[6], us.ccf[5], us.ccf[4], us.ccf[3], us.ccf[2], us.ccf[1]), sep='\n', file=tex.file, append=TRUE)
  }

cat(sprintf('\\bottomrule\n'), file=tex.file, append=TRUE)
cat(sprintf('\\end{tabular}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\end{center}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\end{sidewaystable}\n'), file=tex.file, append=TRUE)

### Write out the after-tax return to `business' capital and tax on household income

cat(sprintf('\\newpage\n'), file=tex.file, append=TRUE)

cat(sprintf('\\begin{table}[htbp]\n'), file=tex.file, append=TRUE)
cat(sprintf('\\centering  \\vskip -1cm\\scriptsize\n'), file=tex.file, append=TRUE)
cat(sprintf('\\caption{U.S.\\@ Return to Capital and Tax Rate on Household Income}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\label{tab:return}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\begin{tabular}{c d{2.2} d{2.2} d{2.2} d{2.2} c d{2.2} d{2.2} d{2.2} d{2.2}} \n'), file=tex.file, append=TRUE)
cat(sprintf('\\toprule\n'), file=tex.file, append=TRUE)
cat(sprintf('& \\multicolumn{4}{c}{Return to Capital} && \\multicolumn{4}{c}{Tax Rate, $\\tau_h$} \\\\\n'), file=tex.file, append=TRUE)
cat(sprintf('\\cmidrule{2-5}  \\cmidrule{7-10}\n'), file=tex.file, append=TRUE)
cat(sprintf('Year & \\multicolumn{1}{c}{Q1}  & \\multicolumn{1}{c}{Q2} & \\multicolumn{1}{c}{Q3} & \\multicolumn{1}{c}{Q4} && \\multicolumn{1}{c}{Q1}  & \\multicolumn{1}{c}{Q2} & \\multicolumn{1}{c}{Q3} & \\multicolumn{1}{c}{Q4}   \\\\\n'), file=tex.file, append=TRUE)
cat(sprintf('\\midrule\n'), file=tex.file, append=TRUE)

T = 64

for (i in 1:T)
  {
    j <- (i-1)*4 + 28
    cat(sprintf('%4i & %6.2f & %6.2f & %6.2f & %6.2f && %6.2f & %6.2f & %6.2f & %6.2f\\\\\n',
                i+1953,
                quarter_return_business_capital_after_tax[j+1],
                quarter_return_business_capital_after_tax[j+2],
                quarter_return_business_capital_after_tax[j+3],
                quarter_return_business_capital_after_tax[j+4],
                100*quarter_tau_h[j+1],
                100*quarter_tau_h[j+2],
                100*quarter_tau_h[j+3],
                100*quarter_tau_h[j+4]), ### 2017-03-27: changed "j+14" to "j+4"
        file=tex.file, append=TRUE)
  }

cat(sprintf('\\bottomrule\n'), file=tex.file, append=TRUE)
cat(sprintf('\\thispagestyle{empty}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\end{tabular}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\end{table}\n'), file=tex.file, append=TRUE)

### Write out the tax rates on capital and labor income

cat(sprintf('\\begin{table}[htbp]\n'), file=tex.file, append=TRUE)
cat(sprintf('\\centering  \\vskip -1cm\\scriptsize\n'), file=tex.file, append=TRUE)
cat(sprintf('\\caption{U.S.\\@ Tax Rates on Labor and Capital Income}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\label{tab:return}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\begin{tabular}{c d{2.2} d{2.2} d{2.2} d{2.2} c d{2.2} d{2.2} d{2.2} d{2.2}} \n'), file=tex.file, append=TRUE)
cat(sprintf('\\toprule\n'), file=tex.file, append=TRUE)
cat(sprintf('& \\multicolumn{4}{c}{Tax Rate, $\\tau_n$} && \\multicolumn{4}{c}{Tax Rate, $\\tau_k$} \\\\\n'), file=tex.file, append=TRUE)
cat(sprintf('\\cmidrule{2-5}  \\cmidrule{7-10}\n'), file=tex.file, append=TRUE)
cat(sprintf('Year & \\multicolumn{1}{c}{Q1}  & \\multicolumn{1}{c}{Q2} & \\multicolumn{1}{c}{Q3} & \\multicolumn{1}{c}{Q4} && \\multicolumn{1}{c}{Q1}  & \\multicolumn{1}{c}{Q2} & \\multicolumn{1}{c}{Q3} & \\multicolumn{1}{c}{Q4}   \\\\\n'), file=tex.file, append=TRUE)
cat(sprintf('\\midrule\n'), file=tex.file, append=TRUE)

for (i in 1:T)
  {
    j = (i-1)*4 + 28;
    cat(sprintf('%4i & %6.2f & %6.2f & %6.2f & %6.2f && %6.2f & %6.2f & %6.2f & %6.2f\\\\\n',
                i+1953,
                100*quarter_tau_n[j+1],
                100*quarter_tau_n[j+2],
                100*quarter_tau_n[j+3],
                100*quarter_tau_n[j+4],
                100*quarter_tau_k[j+1],
                100*quarter_tau_k[j+2],
                100*quarter_tau_k[j+3],
                100*quarter_tau_k[j+4]),
        file=tex.file, append=TRUE)
  }

cat(sprintf('\\bottomrule\n'), file=tex.file, append=TRUE)
cat(sprintf('\\thispagestyle{empty}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\end{tabular}\n'), file=tex.file, append=TRUE)
cat(sprintf('\\end{table}\n'), file=tex.file, append=TRUE)


cat(sprintf('\\end{document}\n'), file=tex.file, append=TRUE)

### Write the raw data

us.data <- merge(quarter_real_pc_y, 
	   quarter_real_pc_cm, 
	   quarter_real_pc_xs+quarter_real_pc_xe + quarter_real_pc_xh+quarter_real_pc_xd,  
	   quarter_real_pc_xs+quarter_real_pc_xe, 
	   quarter_real_pc_xh+quarter_real_pc_xd,  
	   quarter_pc_hours, 
	   quarter_real_pc_y/quarter_pc_hours, 
	   quarter_real_pc_ks+quarter_real_pc_ke + quarter_real_pc_kh+quarter_real_pc_kd, 
	   quarter_real_pc_ks+quarter_real_pc_ke, 
	   quarter_real_pc_kh+quarter_real_pc_kd, 
	   quarter_solow_residual, 
	   quarter_relative_price_investment, 
	   quarter_tau_k, 
	   quarter_tau_n, 
	   quarter_return_business_capital_pre_tax, 
	   quarter_return_business_capital_pre_tax_no_gain, 
	   quarter_return_business_capital_after_tax, 
	   quarter_return_business_capital_after_tax_no_gain, 
	   quarter_return_all_capital_pre_tax, 
	   quarter_return_all_capital_pre_tax_no_gain, 
	   quarter_return_all_capital_after_tax, 
	   quarter_return_all_capital_after_tax_no_gain, 
	   quarter_return_housing_capital_pre_tax, 
	   quarter_return_housing_capital_pre_tax_no_gain, 
	   quarter_return_housing_capital_after_tax, 
	   quarter_return_housing_capital_after_tax_no_gain)

##############################################################################
#
#html.file <- 'us-moments.html'
#
#if (file.exists(html.file))
#  {
#    file.remove(html.file)
#  }
#
#cat(sprintf('<HTML>\n'), file=html.file, append=TRUE)
#cat(sprintf('<HEAD>\n'), file=html.file, append=TRUE)
#cat(sprintf('<TITLE>U.S. Business Cycle Moments</TITLE>\n'), file=html.file, append=TRUE)
#cat(sprintf('</HEAD>\n'), file=html.file, append=TRUE)
#cat(sprintf('<BODY>\n'), file=html.file, append=TRUE)
#cat(sprintf('<H1>Hodrick-Prescott Filtered U.S. Business Cycle Moments</H1>\n'), file=html.file, append=TRUE)
#
#cat(sprintf('<TABLE style="width:100%>\n'), file=html.file, append=TRUE)
#
#cat(sprintf('<caption>U.S.\\@ %s--%s: Selected Moments</caption>\n', t.start, t.end), file=html.file, append=TRUE)
#
#cat(sprintf('<tr>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>Standard</th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th colspan="9">Cross Correlation of Real Output With</th>\n'), file=html.file, append=TRUE)
#cat(sprintf('</tr>\n'), file=html.file, append=TRUE)
#
#cat(sprintf('<tr>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>Deviation</th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t-4</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t-3</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t-2</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t-1</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t+1</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t+2</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t+3</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('<th>x<sub>t+4</sub></th>\n'), file=html.file, append=TRUE)
#cat(sprintf('</tr>\n'), file=html.file, append=TRUE)
#
#for (i in 1:N)
#  {
#    us.sd <- 100*sqrt(var(as.numeric(us.data.filt[,i])))
#    us.ac <- cor(us.data.filt[1:(T-1),i], us.data.filt[2:T,i])
#    zzz <- ccf(as.ts(us.data.filt[,1]), as.ts(us.data.filt[,i]), lag.max =4)
#    us.ccf <- as.numeric(zzz$acf)
#    cat(sprintf("<tr><td>%-50s </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td> <td> %8.2f </td></tr>", us.sstrg[i], us.sd, us.ccf[9], us.ccf[8], us.ccf[7], us.ccf[6], us.ccf[5], us.ccf[4], us.ccf[3], us.ccf[2], us.ccf[1]), sep='\n', file=html.file, append=TRUE)
#  }
#
#cat(sprintf('</TABLE>\n'), file=html.file, append=TRUE)
#cat(sprintf('</BODY>\n'), file=html.file, append=TRUE)
#cat(sprintf('</HTML>\n'), file=html.file, append=TRUE)
#
##############################################################################



# 2017-03-27: Added proper names
names(us.data) <- c("Output",
                    "Consumption",
                    "Investment",
                    "Market investment",
                    "Home investment",
                    "Hours",
                    "Productivity",
                    "Capital stock",
                    "Capital stock: market",
                    "Capital stock: home",
                    "Solow residual",
                    "Relative price of investment",
                    "Tax rate: capital income",
                    "Tax rate: labor income",
                    "Return to business capital, pre-tax",
                    "Return to business capital, pre-tax, no capital gain",
                    "Return to business capital, after-tax",
                    "Return to business capital, after-tax, no capital gain",
                    "Return to all capital, pre-tax",
                    "Return to all capital, pre-tax, no capital gain",
                    "Return to all capital, after-tax",
                    "Return to all capital, after-tax, no capital gain",
                    "Return to housing capital, pre-tax",
                    "Return to housing capital, pre-tax, no capital gain",
                    "Return to housing capital, after-tax",
                    "Return to housing capital, after-tax, no capital gain")
                    

#write.table(us.data, file='usdata.csv', row.names=TRUE, col.names=TRUE, sep=',', na='NaN')
write.zoo(us.data, file='usdata.csv', sep=',', na='NaN')
write.zoo(us.data, file='usdata.dat') # 2017-03-27: write all the data (with dates)

write.zoo(annual_alpha, file='alpha.dat', sep='\t', na='?', col.names=FALSE)

### Write the `shocks' to file

shocks <- merge(quarter_relative_price_investment, 
                quarter_solow_residual,
                quarter_tau_n,
                quarter_tau_k)

write.table(shocks, file='shocks.csv', row.names=FALSE, col.names=FALSE, sep=',', na='NaN')

### Regression of Solow residual against its lag and a time trend.
### Drop observations up to the Korean War

log_quarter_solow_residual <- log(quarter_solow_residual)
x <- lag(log_quarter_solow_residual)
y <- log_quarter_solow_residual
y <- y[paste(tKW,end(quarter_solow_residual),sep='/')]
x <- x[paste(tKW,end(quarter_solow_residual),sep='/')]

#y <- log(quarter_solow_residual[paste('1953-10-01',end(quarter_solow_residual),sep='/')])
names(y) <- "Solow"
names(x) <- "Solow lag"
trend <- seq(1, length(y))
X <- merge(x, trend)

print("Solow residual regression")

print(lm(y ~ X))

print(" *** Finished ***")
print("See the files us-moments.tex, usdata.csv and shocks.csv")
