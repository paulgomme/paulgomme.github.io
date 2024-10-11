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
### Matlab code to R ensured and this file is the end product. More recently,
### I have translated the R code into Python.  Thanks to Lin Shao, then of
### the St. Louis Fed, for figuring out most of the FRED codes corresponding
### to the data below.

### Kindly report any errors to paul.gomme@concordia.ca

import numpy as np
import pandas as pd
import pandas_datareader as pdr
import datetime
import copy
from tabulate import tabulate
import os
import math
from scipy.optimize import fsolve
from statsmodels.tsa.filters.hp_filter import hpfilter
import statsmodels.formula.api as sm
import matplotlib.pyplot as plt
import matplotlib as mpl
from cycler import cycler
###import time

### 2021-04-02: Get longer time series for the capital income tax rate (and
### so for after-tax returns), by copying the last observation for certain
### housing income series. To get these longer series, set f_extend=True.
### Those who prefer to only use actual data should ensure f_extend=False.
f_extend = False
#f_extend = True

### "smoothing" controls the type of interpolation used for annual to
### quarterly conversions. Good choices include 'linear', 'cubic' and
### 'quadratic'. If blank, uses linear.
smoothing = 'linear'

pd.set_option('display.max_rows', None)

mpl.rcParams['axes.prop_cycle'] = cycler(color=['blue', 'black', 'red', '#005000', 'purple', 'darkred', 'chocolate', 'royalblue', 'navy', 'darkgreen', 'pink'])
mpl.rcParams['axes.spines.right'] = False
mpl.rcParams['axes.spines.top'] = False
mpl.rcParams['axes.xmargin'] = 0
mpl.rcParams['axes.ymargin'] = 0.01
mpl.rcParams["figure.figsize"] = (20,12)
mpl.rcParams['font.family'] = 'sans-serif'
mpl.rcParams['font.size'] = 30
#mpl.rcParams['lines.linewidth'] = 10
mpl.rc('lines', linewidth=8)

### The annual data

annual_fred_map = {
    'A2007C1A027NBEA':	'housing_output', # Housing Output (Bil.$)
    'A2009C1A027NBEA':	'gross_housing_value_added', # Gross Housing Value Added (Bil.$)
    'B952RC1A027NBEA':	'net_housing_value_added', # Net Housing Value Added (Bil.$)
    'B1033C1A027NBEA':	'housing_compensation_of_employees', # Housing: Compensation of Employees (Bil.$)
    'B1031C1A027NBEA':	'housing_taxes_on_production', # Housing: Taxes on Production and Imports (Bil.$)
    'W154RC1A027NBEA':	'housing_subsidies', # Housing Subsidies (Bil.$)
    'W165RC1A027NBEA':	'housing_net_operating_surplus', # Housing Net Operating Surplus (Bil.$)
    'B1037C1A027NBEA':	'housing_net_interest', # Housing: Net Interest (Bil.$)
    'W166RC1A027NBEA':	'housing_NOS_transfer_payments', # Housing Net Operating Surplus: Current Transfer Payments (Bil.$)
    'B1034C1A027NBEA':	'housing_proprietors_income', # Housing: Proprietors' Income with IVA & CCAdj (Bil.$)
    'B1035C1A027NBEA':	'housing_rental_income', # Housing: Rental Income of Persons w/CCadj (Bil.$)
    'B1036C1A027NBEA':	'housing_corp_profits', # Housing: Corporate Profits w/IVA & CCadj (Bil.$)
    'W153RC1A027NBEA':	'housing_NOS_gov_enterprises', # Housing Net Operating Surplus: Current Surplus of Govt Enterprises (Bil.$)
    ### See discussion preceeding calculation of quarter_tau_k
    ###    'S230401A027NBEA', # State/Local Govt: Other Taxes (Bil.$)
    'B235RC1A027NBEA':	'gov_custom_duties', # Federal government current tax receipts: Taxes on production and imports: Customs duties
    'B234RC1A027NBEA':	'gov_excise_taxes', # Federal government current tax receipts: Taxes on production and imports: Excise taxes
    'ASLSTAX':	'state_local_sales_taxes', # State and Local Government: Taxes on production and imports: Sales Taxes
    'A033RC1A027NBEA':	'compensation_of_employees', # National income: Compensation of employees
    #'W209RC1A027NBEA', # Compensation of Employees (Bil.$) <-- DISCONTINUED; use previous entry
    #'A553RC1A027NBEA', # Government Wages and Salaries (Bil.$) *** DISCONTINUED; use next entry
    'B202RC1A027NBEA':	'gov_wages_and_salaries', # Government Wages and Salaries (Bil.$)
    'A048RC1A027NBEA':	'rental_income', # Rental Income of Persons with CCAdj (Bil.$)
    'A051RC1A027NBEA':	'corporate_profits', # Corporate Profits with IVA and CCAdj (Bil.$)
    'W255RC1A027NBEA':	'net_interest', # Net Interest and Miscellaneous Payments (Bil.$)
    ###'B1035C1A027NBEA', # Housing: Rental Income of Persons w/CCadj (Bil.$) -- duplicate
    ###'B1036C1A027NBEA', # Housing: Corporate Profits w/IVA & CCadj (Bil.$) --duplicate
    ###'B1037C1A027NBEA', # Housing: Net Interest (Bil.$) -- duplicate
    'GNPA':		'GNP', # Gross National Product (Bil.$)
    'GDPA':		'GDP', # Gross Domestic Product (Bil.$)
    'A765RC1A027NBEA':	'gross_value_added_gov', # Gross Value Added: General Government (Bil.$)
    'A027RC1A027NBEA':	'net_national_product', # Net National Product (Bil.$)
    'A194RC1A027NBEA':	'gov_net_national_product', # General Government: Net Domestic Product (Bil.$)
    'K1NTOTL1EQ000':	'net_stock_private_equipment_software', # Net Stock: Private Fixed Nonresidential Equipment (Mil.$)
    'K1NTOTL1ST000':	'net_stock_private_nonresidential_structures', # Net Stock: Private Fixed Nonresidential Structures (Mil.$)
    'K1R53101ES000':	'net_stock_private_residential_structures', # Net Stock: Private Residential Fixed Assets (Mil.$)
    'K1CTOTL1CD000':	'net_stock_consumer_durables', # Net Stock: Consumer Durable Goods (Mil.$)
    'K1GTOTL1EQ000':	'net_stock_gov_nonresidential_equipment_software', # Net Stock: Government Nonresidential Equipment (Mil.$)
    'K1GTOTL1STNR0':	'net_stock_gov_nonresidential_structures', # Net Stock: Govt Nonresidential Structures (Mil.$)
    'K1GTOTL1SA000':	'net_stock_gov_residential_structures', # Net Stock: Government Residential Fixed Assets (Mil.$)
    'PCNDA':		'PCE_nondurables', # Personal Consumption Expenditures: Nondurable Goods (Bil.$)
    'DNDGRG3A086NBEA':	'DNDGRG3A086NBEA', # Personal consumption expenditures: Nondurable goods (chain-type price index)
    'PCESVA':		'PCE_services', # Personal Consumption Expenditures: Services (Bil.$)
    'DSERRG3A086NBEA':	'DSERRG3A086NBEA', # Personal Consumption Expenditures: Services (chain-type price index)
    'LABSHPUSA156NRUG':	'LABSHPUSA156NRUG', # Labor share (Feenstra et al.)
    'K100071A027NBEA':	'private_inventories', # Produced Assets Net Stock: Private Inventories (Bil.$)
    'K160421A027NBEA':	'private_inventories_neutral_holding_gains_losses', # Neutral Holding Gains or Losses[-]: Private Inventories (Bil.$)
    'K160471A027NBEA':	'private_inventores_real_holding_gains_losses', # Real Holding Gains or Losses[-]: Private Inventories (Bil.$)
    'M1NTOTL1EQ000':	'depreciation_private_equipment_software', # Depreciation: Pvt Nonres Fxd Assets: Equip (Bil.$)
    'M1NTOTL1ST000':	'depreciation_private_nonresidential_structures', # Depreciation: Pvt Nonres Fxd Assets: Struct (Bil.$)
    'M1R53101ES000':	'depreciation_private_residential_structures', # Depreciation: Res Fixed Assets: Private (Bil.$)
    'M1GTOTL1EQ000':	'depreciation_gov_equipment_software', # Depreciation: Gov Nonres Fixed Assets: Equipment (Bil.$)
    'M1GTOTL1STNR0':	'depreciation_gov_nonresidential_structures', # Depreciation: Gov Nonresidential Fixed Assets: Structures (Bil.$)
    'M1GTOTL1SA000':	'depreciation_gov_residential_structures', # Depreciation: Res Fixed Assets: Government (Bil.$)
    'M1CTOTL1CD000':	'depreciation_consumer_durables', # Depreciation: Consumer Durable Goods (Bil.$)
    #'A553RC1A027NBEA', # Government Wages and Salaries (Bil.$) # duplicate??
    'A955RC1A027NBEA':	'gov_consumption_expenditures', # Government consumption expenditures# Government consumption expenditures
    'B009RC1A027NBEA':	'private_investment_nonresidential_structures', # Private Nonresidential Investment: Structures (Bil.$)# Private Nonresidential Investment: Structures (Bil.$)
    'Y033RC1A027NBEA':	'private_investment_equipment_software', # Private Nonresidential Fixed Investment: Equipment (Bil.$)# Private Nonresidential Fixed Investment: Equipment (Bil.$)
    'S210401A027NBEA':	'state_local_property_taxes', # Personal current taxes: State and local: Property taxes# Personal current taxes: State and local: Property taxes
    'ASLPTAX':		'state_local_gov_property_taxes', # State and Local Government: Taxes on production and imports: Property Taxes# State and Local Government: Taxes on production and imports: Property Taxes
    'W837RC1A027NBEA':	'W837RC1A027NBEA', # Local government current tax receipts: Taxes on production and imports: Property taxes# Local government current tax receipts: Taxes on production and imports: Property taxes
    'W737RC1A027NBEA':	'W737RC1A027NBEA', # State government current tax receipts: Taxes on production and imports: Property taxes
    'PRFIA':		'private_investment_residential_structures', # Private Residential Investment (Bil.$)
    'PCDGA':		'PCE_durables', # Personal Consumption Expenditures: Durable Goods (Bil.$)
    'PNFIA':		'private_nonresidential_fixed_investment', # Private Nonresidential Fixed Investment (Bil.$)
    'B008RG3A086NBEA':	'deflator_investment', # Real private fixed investment: Nonresidential (chain-type price index) (2009=100)
    'PNFICA':		'real_private_nonresidential_fixed_investment',	 # Real Private Nonresidential Fixed Investment (Bil.Chn.2009$)
    }

## Once FRED is up again, look at the "Taxes on production and imports:
## Property taxes" series. It appears that the sum (state and local) has been
## discontinued (at both quarterly and annual frequency), but constituent
## parts are available (at least annually).

annual = pdr.DataReader(list(annual_fred_map.keys()), 'fred', datetime.datetime(1900,1,1), api_key=os.getenv('FRED_API_KEY'))
annual.rename(columns=annual_fred_map, inplace = True)
annual = annual.asfreq('YS') # 2024-10-11

annual['real_PCE_nondurables'] = annual['PCE_nondurables'] * 100 / annual['DNDGRG3A086NBEA']
annual['real_PCE_services'] = annual['PCE_services'] * 100 / annual['DSERRG3A086NBEA']
annual['state_local_gov_property_taxes_new'] = annual['W837RC1A027NBEA'] + annual['W737RC1A027NBEA']

######quarter.gov_consumption_expenditures <- A955RC1Q027SBEA

### The quarterly data

quarter_fred_map = {
    'W055RC1Q027SBEA':	'personal_taxes', # Personal Current Taxes (SAAR, Bil.$)
    'W025RC1Q027SBEA':	'gov_taxes_corp_income', # Government Tax Receipts on Corporate Income (SAAR, Bil.$)
    'B249RC1Q027SBEA':	'state_local_gov_property_taxes', # State & Local Government Property Tax Receipts (SAAR, Bil.$)
    'S210400':		'state_local_property_taxes', # State & Local Property Taxes (SAAR, Mil.$)
    'B248RC1Q027SBEA':	'state_local_sales_taxes', # State & Local Sales Taxes
    'B234RC1Q027SBEA':	'gov_excise_taxes', # Federal Government Excise Taxes
    'B235RC1Q027SBEA':	'gov_custom_duties', # Federal Government Custom Duties
    'GDICOMP':		'compensation_of_employees', # Compensation of Employees, Paid: Domestic Employers (SAAR, Bil.$)
    #'A576RC1Q027SBEA', # Wages & Salaries (SAAR, Bil.$) *** DISCONTINUED - use WASCUR
    'WASCUR':		'wage_salary_accruals', # 2019-03-02# Wages & Salaries (SAAR, Bil.$)
    'W272RC1Q027SBEA':	'net_interest', # Net Interest & Miscellaneous Payments: Domestic Industries (SAAR, Bil.$)
    'PROPINC':		'proprietors_income', # Proprietors' Income with IVA and CCAdj (SAAR, Bil.$)
    'RENTIN':		'rental_income', # Rental Income of Persons with CCAdj (SAAR, Bil.$)
    'A445RC1Q027SBEA':	'corp_profits', # Corporate Profits with IVA & CCAdj: Domestic Industries (SAAR, Bil.$)
    'B029RC1Q027SBEA':	'business_transfer_payments', # Business Current Transfer Payments (SAAR, Bil.$)
    'W260RC1Q027SBEA':	'net_operating_surplus_private', # Net Operating Surplus: Private Enterprises (SAAR, Bil.$)
    'B039RC1Q027SBEA':	'employer_contributions_gov_social_insurance', # Employer Contributions for Government Social Insurance (SAAR, Bil.$)
    'A061RC1Q027SBEA':	'contributions_gov_social_insurance', # Contributions for Government Social Insurance (SAAR, Bil.$)
    'A024RC1Q027SBEA':	'private_capital_consumption', # Consumption of Private Fixed Capital (SAAR, Bil.$)
    'W276RC1Q027SBEA':	'consumption_fixed_capital_business', # Consumption of Fixed Capital: Domestic Business (Bil.$)
    'CBI':		'change_private_inventories', # Change in Private Inventories (SAAR, Bil.$)
    'A2009C1Q027SBEA':	'gross_housing_value_added', # Gross Housing Value Added (SAAR, Bil.$)
    #    'A024RC1Q027SBEA', # Consumption of fixed capital: Private
    'B2607C1Q027SBEA':	'private_capital_consumption_housing', # Consumption of fixed capital: Private: Households and institutions: Owner-occupied housing
    'USRECQ':		'NBER', # Quarterly NBER Recession/Expansion: Recession Shading (+1/-1)
    'PCND':		'PCE_nondurables', # Personal Consumption Expenditures: Nondurable Goods (SAAR, Bil.$)
    'DNDGRG3Q086SBEA':	'PCE_nondurables_price', # Personal consumption expenditures: Nondurable goods (chain-type price index), 2009-100
    'PCESV':		'PCE_services', # Personal Consumption Expenditures: Services (SAAR, Bil.$)
    'DSERRG3Q086SBEA':	'PCE_services_price', # Personal consumption expenditures: Services (chain-type price index), 2009=100
    'GDP':		'GDP', # Gross Domestic Product (SAAR, Bil.$)
    'DHUTRC1Q027SBEA':	'PCE_housing_services', # 	PCE: Housing Services and utilities (starts 1959I)
    #'A553RC1Q027SBEA', # Government Wages and Salaries (SAAR, Bil.$) *** DISCONTINUED; use next entry?
    'B202RC1Q027SBEA':	'gov_wages_and_salaries', # 2018-03-02# Government Wages and Salaries (Bil.$)
    'A955RC1Q027SBEA':	'gov_consumption_expenditures', # Government consumption expenditures
    #    'DHSGRC1A027NBEA', # (only annual data)	Personal Consumption Expenditures: Housing (SAAR, Mil.$)
    'B009RC1Q027SBEA':	'private_investment_nonresidential_structures', # Private Nonresidential Investment: Structures (SAAR, Bil.$)
    'Y033RC1Q027SBEA':	'private_investment_equipment_software', # Private Nonresidential Fixed Investment: Equipment (SAAR, Bil.$)
    'PRFI':		'private_investment_residential_structures', # Private Residential Investment (SAAR,Bil.$)
    'PCDG':		'PCE_durables', # Personal Consumption Expenditures: Durable Goods (SAAR, Bil.$)
    'PRS85006173':	'labor share non-farm business',      # Labor share, non-farm business sector (2012=100)
    'PRS84006173':	'labor share business',      # Labor share, business sector (2012=100)
    'PRSCQ':		'hours_private_nonfarm', # Aggregate Hours: Nonfarm Payrolls, Private Sector (SAAR, Bil.Hrs) *** Discontinued
    'A760RC1Q027SBEA':	'gov_gross_investment_structures', # Government Gross Investment in Structures (SAAR, Bil.$)
    'Y054RC1Q027SBEA':	'gov_gross_investment_equipment_software', # Government Gross Investment in Equipment (SAAR, Bil.$)
    'PNFI':		'private_nonresidential_fixed_investment', # Private Nonresidential Fixed Investment
    'B008RG3Q086SBEA':	'real_private_nonresidential_fixed_investment_price', # Real private fixed investment: Nonresidential (chain-type price index), 2009=100
    'A371RC1Q027SBEA':	'private_inventories', # (since 1996; splice in previous Haver data?)	Private Inventories (EOP, SAQT, Bil.$)
    'HOABS':		'hours-business', # Business Sector: Hours worked for all workers
    'HOANBS':		'hours-nonfarm business', # Nonfarm Business Sector: Hours worked for all workers
    }

quarter = pdr.DataReader(list(quarter_fred_map.keys()), 'fred', datetime.datetime(1900,1,1), api_key=os.getenv('FRED_API_KEY'))
quarter.rename(columns=quarter_fred_map, inplace = True)
quarter = quarter.asfreq('QS') # 2024-10-11

quarter['real_PCE_nondurables'] = 100 * quarter['PCE_nondurables'] / quarter['PCE_nondurables_price']
quarter['real_PCE_services'] = 100 * quarter['PCE_services'] / quarter['PCE_services_price']

month_fred = ["CPIAUCSL",	# CPI-U: All Items (SA, 1982-84=100) (monthly)
              "CNP16OV"	# Civilian Noninstitutional Population: 16 Years and Over (NSA, Thous) (monthly)
              ]

month = pdr.DataReader(month_fred, 'fred', datetime.datetime(1900,1,1), api_key=os.getenv('FRED_API_KEY'))

### Q means convert to quarterly
### S means use start of period
converted = month.resample('QS').mean()

quarter['CPI_all_items'] = converted['CPIAUCSL']
quarter['civilian_noninstitutional_pop_16_plus'] = converted['CNP16OV']

### FRED does not provide very long series for many real variables. Obtain
### longer data series by dividing nominal variables by the corresponding
### price index.

quarter['real_private_nonresidential_fixed_investment'] \
    = quarter['private_nonresidential_fixed_investment'] * 100 \
    / quarter['real_private_nonresidential_fixed_investment_price']

### Definitions of: price of consumption goods (nondurables + services); and
### the relative price of investment goods.

annual['deflator_consumption'] = 100 * (annual['PCE_nondurables'] \
                                        + annual['PCE_services']) \
                                        / (annual['real_PCE_nondurables'] \
                                           + annual['real_PCE_services'])
annual['relative_price_investment'] = annual['deflator_investment'] \
    / annual['deflator_consumption']

### Annual real capital stocks

annual['real_ke'] = 100 * annual['net_stock_private_equipment_software'] \
    / annual['deflator_consumption']
annual['real_ks'] = 100 * annual['net_stock_private_nonresidential_structures'] \
    / annual['deflator_consumption']
annual['real_kh'] = 100 * annual['net_stock_private_residential_structures'] \
    / annual['deflator_consumption']
annual['real_kd'] = 100 * annual['net_stock_consumer_durables'] \
    / annual['deflator_consumption']
annual['real_g_ke'] = 100 * annual['net_stock_gov_nonresidential_equipment_software'] \
    / annual['deflator_consumption']
annual['real_g_ks'] = 100 * (annual['net_stock_gov_nonresidential_structures'] \
                             + annual['net_stock_gov_residential_structures']) \
                             / annual['deflator_consumption']

### Annual depreciation rates

annual['delta_ke'] = annual['depreciation_private_equipment_software'] \
    / annual['net_stock_private_equipment_software'].shift(1)

annual['delta_ks'] = annual['depreciation_private_nonresidential_structures'] \
    / annual['net_stock_private_nonresidential_structures'].shift(1)

annual['delta_kh'] = annual['depreciation_private_residential_structures'] \
    / annual['net_stock_private_residential_structures'].shift(1) 

annual['delta_kd'] = annual['depreciation_consumer_durables'] \
    / annual['net_stock_consumer_durables'].shift(1)

annual['delta_g_ke'] = annual['depreciation_gov_equipment_software'] \
    / annual['net_stock_gov_nonresidential_equipment_software'].shift(1)

annual['delta_g_ks'] = annual['depreciation_gov_nonresidential_structures'] \
    / annual['net_stock_gov_nonresidential_structures'].shift(1)

annual['delta_g_kh'] = annual['depreciation_gov_residential_structures'] \
    / annual['net_stock_gov_residential_structures'].shift(1)

annual['delta_market'] = (annual['depreciation_private_equipment_software'] \
                     + annual['depreciation_private_nonresidential_structures']) \
                     / (annual['net_stock_private_equipment_software'].shift(1) \
                        + annual['net_stock_private_nonresidential_structures'].shift(1))

annual['delta_home'] = (annual['depreciation_consumer_durables'] \
                     + annual['depreciation_private_residential_structures']) \
                     / (annual['net_stock_consumer_durables'].shift(1) \
                        + annual['net_stock_private_residential_structures'].shift(1))  

annual['delta_all'] = (annual['depreciation_private_equipment_software'] \
                       + annual['depreciation_private_nonresidential_structures'] \
                       + annual['depreciation_consumer_durables'] \
                       + annual['depreciation_private_residential_structures']) \
                       / (annual['net_stock_private_equipment_software'].shift(1) \
                          + annual['net_stock_private_nonresidential_structures'].shift(1) \
                          + annual['net_stock_consumer_durables'].shift(1) \
                          + annual['net_stock_private_residential_structures'].shift(1))

### Index for Korean war
tKW = '1954-01-01'

#Average depreciation rates since Korean War
delta_all_annual = annual['delta_all'].loc[tKW:].mean()
delta_market_annual = annual['delta_market'].loc[tKW:].mean()
delta_market_equipment_annual = annual['delta_ke'].loc[tKW:].mean()
delta_market_structures_annual = annual['delta_ks'].loc[tKW:].mean()
delta_home_annual = annual['delta_home'].loc[tKW:].mean()
delta_home_structures_annual = annual['delta_kh'].loc[tKW:].mean()
delta_home_durables_annual = annual['delta_kd'].loc[tKW:].mean()
#Average depreciation rates since Korean War (expressed quarterly)
delta_all = 1 - (1 - annual['delta_all'].loc[tKW:].mean())**(.25)
delta_market = 1 - (1 - annual['delta_market'].loc[tKW:].mean())**(.25)
delta_home = 1 - (1 - annual['delta_home'].loc[tKW:].mean())**(.25)

### Compute labor's share of income (alpha). Need to first compute factor
### incomes.

annual['income_labor'] = annual['compensation_of_employees'] - \
  annual['gov_wages_and_salaries'] - \
  annual['housing_compensation_of_employees']
annual['income_capital'] = annual['rental_income'] + \
  annual['corporate_profits'] + annual['net_interest'] - \
  (annual['housing_rental_income'] + annual['housing_corp_profits'] + \
   annual['housing_net_interest']) + annual['GNP'] - \
  annual['gross_value_added_gov'] - annual['gross_housing_value_added'] - \
  (annual['net_national_product'] - annual['gov_net_national_product'] - \
   annual['net_housing_value_added']) 
annual['alpha'] = annual['income_capital'] / (annual['income_capital'] + annual['income_labor']) 
alpha_mean = annual['alpha'].loc[tKW:].mean()

### `Usual' macroaggregates

annual['real_y'] = annual['GDP'] / (annual['deflator_consumption']/100)
annual['real_yp'] = annual['real_y'] - annual['gov_wages_and_salaries'] / \
    (annual['deflator_consumption']/100) 
annual['real_xs'] = annual['private_investment_nonresidential_structures'] / \
    (annual['deflator_consumption']/100) 
annual['real_xe'] = annual['private_investment_equipment_software'] / \
    (annual['deflator_consumption']/100) 
annual['real_xh'] = annual['private_investment_residential_structures'] / \
    (annual['deflator_consumption']/100) 
annual['real_xd'] = annual['PCE_durables'] / (annual['deflator_consumption']/100)

annual['xm_y'] = (annual['private_investment_nonresidential_structures'] \
	          + annual['private_investment_equipment_software']) / annual['GDP']

annual['xh_y'] = (annual['private_investment_residential_structures'] \
	          + annual['PCE_durables']) / annual['GDP']

x_y = ((annual['private_investment_nonresidential_structures'].loc[tKW:] \
        + annual['private_investment_equipment_software'].loc[tKW:] \
        + annual['private_investment_residential_structures'].loc[tKW:] \
        + annual['PCE_durables'].loc[tKW:]) / annual['GDP'].loc[tKW:]).mean()
xm_y = annual['xm_y'].loc[tKW:].mean()
xh_y = annual['xh_y'].loc[tKW:].mean()
xms_y = (annual['private_investment_nonresidential_structures'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean()
xme_y = (annual['private_investment_equipment_software'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean()
xhh_y = (annual['private_investment_residential_structures'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean()
xhd_y = (annual['PCE_durables'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean()

k_y = ((annual['net_stock_private_nonresidential_structures'].loc[tKW:] \
        + annual['net_stock_private_equipment_software'].loc[tKW:] \
        + annual['net_stock_private_residential_structures'].loc[tKW:] \
        + annual['net_stock_consumer_durables'].loc[tKW:]) / annual['GDP'].loc[tKW:]).mean() / 1000
km_y = ((annual['net_stock_private_nonresidential_structures'].loc[tKW:] \
         + annual['net_stock_private_equipment_software'].loc[tKW:]) / annual['GDP'].loc[tKW:]).mean() / 1000
kh_y = ((annual['net_stock_private_residential_structures'].loc[tKW:] \
         + annual['net_stock_consumer_durables'].loc[tKW:]) / annual['GDP'].loc[tKW:]).mean() / 1000
kms_y = (annual['net_stock_private_nonresidential_structures'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean() / 1000
kme_y = (annual['net_stock_private_equipment_software'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean() / 1000
khh_y = (annual['net_stock_private_residential_structures'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean() / 1000
khd_y = (annual['net_stock_consumer_durables'].loc[tKW:] / annual['GDP'].loc[tKW:]).mean() / 1000

quarter['deflator_consumption'] = 100 * \
    (quarter['PCE_nondurables'] + quarter['PCE_services']) / \
    (quarter['real_PCE_nondurables'] + quarter['real_PCE_services']) 
quarter['deflator_investment'] = 100 * \
    quarter['private_nonresidential_fixed_investment'] / \
    quarter['real_private_nonresidential_fixed_investment'] 
quarter['relative_price_investment'] = quarter['deflator_investment'] / \
    quarter['deflator_consumption'] 

quarter = quarter.copy()

### Splice in some `old' data

### Data from Haver (no longer available?)

quarter.loc['1947-01-01': '2004-01-01', 'PCE_housing_services_old'] = [15.1, 15.6, 16.3, 16.9, 17.3, 17.7,
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
	1152.9, 1167.7, 1181.5, 1191.4, 1204.9, 1216.4, 1229.2]

z = quarter['PCE_housing_services'].loc['1959-01-01'] / quarter['PCE_housing_services_old'].loc['1959-01-01']
t1 = '1947-01-01'
t2 = '1958-10-01'

### 2016-08-02 Paul Gomme

### The new housing services data includes utilities. Apparently, the data
### without utilities (which was used in earlier work) is no longer
### available. *sigh* To continue calculations, use the new data, and splice
### in the old data (since the new data does not go back far enough in time),
### adjusting the level to avoid a jump.

#quarter['PCE_housing_services'].loc[t1:t2] = z * quarter['PCE_housing_services_old'].loc[t1:t2]
quarter.loc[t1:t2,'PCE_housing_services'] = z * quarter.loc[t1:t2,'PCE_housing_services_old']


## The BLS only reports hours data since 1964.  Use some older data to get a
## longer time series.

## This is an old series I probably got from the old Citibase

quarter.loc['1947-01-01': '2002-07-01', 'hours_old_lhtpriva'] = [80108, 79880, 80193, 81309, 81673, 81243,
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
	197803, 197486, 197433, 197034]

z = quarter['hours_private_nonfarm'].loc['1964-01-01'] / quarter['hours_old_lhtpriva'].loc['1964-01-01']
t1 = '1947-01-01'
t2 = '1963-10-01'

quarter['hours'] = quarter['hours_private_nonfarm']
quarter.loc[t1:t2,'hours'] = z * quarter.loc[t1:t2,'hours_old_lhtpriva']

### The FRED series PRSCQ ends 2022Q3. After careful consideration of the
### series available from FRED, the series HOABS (Business sector: hours
### worked for all workers) best lines up with our measurement of output (the
### business sector).  HOABS has the added virtue that it starts in 1947, and
### so there is no need to splice in old Citibase data.

quarter['hours'] = quarter['hours-business']

### Nominal private inventories, from Haver.

quarter.loc['1947-01-01': '2015-01-01', 'private_inventories_haver'] = [93.0, 94.2, 99.5, 107.0, 103.5,
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
	2517.5, 2496.0, 2467.9]

t1 = '1947-01-01'
t2 = '1996-07-01'
quarter.loc[t1:t2,'private_inventories'] = quarter.loc[t1:t2,'private_inventories_haver']

### End of splicing

quarter = quarter.copy()

quarter['real_inventories'] = quarter['private_inventories'] / (quarter['deflator_consumption'].shift(-1)/100)

### `Usual' macroaggregates

quarter['real_y'] = (quarter['GDP']  - quarter['PCE_housing_services']) / \
    (quarter['deflator_consumption'] / 100)
quarter['real_yp'] = quarter['real_y'] - quarter['gov_wages_and_salaries'] / \
    (quarter['deflator_consumption'] / 100) 
quarter['real_xs'] = \
    quarter['private_investment_nonresidential_structures'] / \
    (quarter['deflator_consumption'] / 100) 
quarter['real_xe'] = quarter['private_investment_equipment_software'] / \
    (quarter['deflator_consumption'] / 100) 
quarter['real_xh'] = quarter['private_investment_residential_structures'] / \
    (quarter['deflator_consumption'] / 100) 
quarter['real_xd'] = quarter['PCE_durables'] / (quarter['deflator_consumption'] / 100)
quarter['real_cm'] = (quarter['PCE_nondurables'] + quarter['PCE_services'] - \
                      quarter['PCE_housing_services']) / (quarter['deflator_consumption'] / 100) 

### Impute to quarterly. In the earlier spreadsheets and Matlab code, the
### annual observation was simply repeated. Here, I use Python's
### interpolation instead.

quarter['housing_net_operating_surplus'] = (annual['housing_net_operating_surplus'] / annual['gross_housing_value_added']).resample('QS').interpolate(method=smoothing) \
    * quarter.loc['1947-01-01':annual['housing_net_operating_surplus'].last_valid_index(), 'gross_housing_value_added']
quarter['housing_net_interest'] = (annual['housing_net_interest'] / annual['gross_housing_value_added']).resample('QS').interpolate(method=smoothing) \
    * quarter['gross_housing_value_added']
quarter['housing_proprietors_income'] = (annual['housing_proprietors_income'] / annual['gross_housing_value_added']).resample('QS').interpolate(method=smoothing) \
    * quarter['gross_housing_value_added']
quarter['housing_rental_income'] = (annual['housing_rental_income'] / annual['gross_housing_value_added']).resample('QS').interpolate(method=smoothing) \
    * quarter['gross_housing_value_added']
quarter['housing_corp_profits'] = (annual['housing_corp_profits'] / annual['gross_housing_value_added']).resample('QS').interpolate(method=smoothing) \
    * quarter['gross_housing_value_added']

### Optionally, fill NaN at end of series with last actual observation
if f_extend:
    quarter['housing_net_operating_surplus'].fillna(method='ffill', inplace=True)
    quarter['housing_net_interest'].fillna(method='ffill', inplace=True)
    quarter['housing_proprietors_income'].fillna(method='ffill', inplace=True)
    quarter['housing_rental_income'].fillna(method='ffill', inplace=True)
    quarter['housing_corp_profits'].fillna(method='ffill', inplace=True)

### 2021-02-24: annual.state_local_property_taxes available to 2020; earlier series only to 2019

quarter['state_local_property_taxes_spline'] = annual['state_local_property_taxes'].resample('QS').interpolate(method=smoothing)
quarter['property_taxes_household'] = quarter['state_local_property_taxes'] / 1000
quarter.loc['1929-01-01': '1987-10-01', 'property_taxes_household'] = quarter.loc['1929-01-01': '1987-10-01', 'state_local_property_taxes_spline']

quarter['state_local_gov_property_taxes_spline'] = annual['state_local_gov_property_taxes'].resample('QS').interpolate(method=smoothing)
quarter['real_estate_property_taxes'] = quarter['state_local_gov_property_taxes']
quarter.loc['1929-01-01': '1957-10-01', 'real_estate_property_taxes'] = quarter.loc['1929-01-01': '1987-10-01', 'state_local_gov_property_taxes_spline']

### 2021-04-01 start

### 2021: The "state and local government property taxes" series I was using
### appears to have been terminated. In its place, I'm using the sum of
### "state government property taxes" and "local government property taxes".

###annual['state_local_gov_property_taxes_new'] = annual['W837RC1A027NBEA'] + annual['W737RC1A027NBEA']
###quarter['state_local_gov_property_taxes_new_spline'] = annual['state_local_gov_property_taxes_new'].resample('QS').interpolate(method=smoothing)
###quarter['real_estate_property_taxes'].loc['2018-01-04':] = quarter['state_local_gov_property_taxes_new_spline'].loc['2018-01-04':]
###f_extend:
### quarter['real_estate_property_taxes'].fillna(method='ffill', inplace=True)

### 2021-04-01 end

### 2019-07-03: See discussion below in calculation of quarter_tau_k

###quarter['other_taxes'] = annual['state_local_gov_other_taxes'].resample('QS').interpolate(method=smoothing)

###def FIND_DELTA(delta_in):
###    quarter[my_delta] = delta_in
###    quarter[my_delta].fillna(method='ffill', inplace=True)
###    quarter[my_capital] = annual[my_capital].loc['1947-01-01']
###    daterange = pd.date_range('1947-01-01', '2022-01-01', freq='QS').strftime('%Y-%m-%d')
###    for t in daterange[2:]:
###        quarter.loc[t, my_capital] = (1.0 - quarter.shift(1).loc[t, my_delta]) * quarter.shift(1).loc[t, my_capital] + quarter.shift(1).loc[t, my_investment]
###    #retval = quarter[my_capital].loc['1947-01-01':] - annual[my_capital]
###    retval = quarter[my_capital] - annual[my_capital]
###    retval.dropna(inplace = True)
###    return retval

def FIND_DELTA(delta_in):
    T = len(investment)

    delta = copy.copy(delta_in)

    qcapital[0] = acapital[0]

    retval = np.zeros(len(delta_in))

    iyear = 0
    iquarter = 0

    for t in range(T):
        qcapital[t+1] = (1.0 - delta[iyear]) * qcapital[t] + investment[t]
        iquarter = iquarter + 1
        if iquarter > 3:
            if iyear+1 < len(acapital):
                retval[iyear] = qcapital[t+1] - acapital[iyear+1]
            iyear = min(iyear + 1, len(acapital)-1)
            iquarter = iquarter - 4

    retval[-1] = delta[-1] - delta[-2]
    return retval

qnew = pd.DataFrame()
anew = pd.DataFrame()

for s in ['e', 's', 'h', 'd']:
    my_delta = 'delta_' + s
    my_investment = 'real_x' + s
    my_capital = 'real_k' + s

    ### 2024-04-17: Capital data is in millions of dollars; NIPA data in billions.
    acapital = copy.copy(annual.loc['1946-01-01':, my_capital]).dropna(inplace = False).to_numpy() / 1000
    investment = copy.copy(quarter.loc['1947-01-01':, my_investment]).dropna(inplace = False).to_numpy()
    qcapital = np.zeros(len(investment)+1)
    guess = 0.03 * np.ones(len(acapital))

    ans = fsolve(FIND_DELTA, guess)

    ### Old code
    ###lastdate = quarter[my_investment].shift(1).last_valid_index().strftime('%Y-%m-%d')
    ###quarter.loc['1947-01-01': lastdate, my_capital] = qcapital#[:-1]
    ###annual.loc['1947-01-01':, my_delta] = ans[:-1]
    ### New code (replaces previous 3 lines). 2024-10-11
    rng = pd.date_range('1947-01-01', periods = qcapital.size, freq = 'QS')
    df = pd.DataFrame({my_capital: qcapital}, index = rng)
    qnew = pd.concat([qnew, df], axis=1)
    rng = pd.date_range('1947-01-01', periods = ans.size, freq = 'YS')
    df = pd.DataFrame({my_delta: ans}, index = rng)
    anew = pd.concat([anew, df], axis=1)

quarter = pd.concat([quarter, qnew], axis=1)
annual = pd.concat([annual, anew], axis=1)

################################################################################

### Calculations of tax rates as in Mendoza, Razin and Tesar

quarter['real_estate_taxes_business'] = \
    quarter['real_estate_property_taxes'] * quarter['real_ks'] / \
    (quarter['real_ks'] + quarter['real_kh'])
quarter['real_estate_taxes_household'] = \
    quarter['real_estate_property_taxes'] * quarter['real_kh'] / \
    (quarter['real_ks'] + quarter['real_kh'])

quarter['business_income_pre_tax'] = \
    quarter['net_operating_surplus_private'] - \
    quarter['housing_net_operating_surplus'] - \
    (1-alpha_mean)*(quarter['proprietors_income'] - \
	            quarter['housing_proprietors_income']) 

quarter['tau_h'] = quarter['personal_taxes'] / \
    (quarter['wage_salary_accruals'] + quarter['net_interest'] + \
     quarter['proprietors_income'] + quarter['rental_income'])

quarter['tau_n'] = (quarter['tau_h']*(quarter['wage_salary_accruals'] + \
				      (1-alpha_mean)*quarter['proprietors_income']) \
		    + quarter['contributions_gov_social_insurance']) / \
                    (quarter['wage_salary_accruals'] + \
                     (1-alpha_mean)*quarter['proprietors_income'] + \
                     quarter['employer_contributions_gov_social_insurance']) 

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

###quarter_tau_k = (quarter_tau_h*(quarter.net_interest + 
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

quarter['tau_k'] = (quarter['tau_h'] * (quarter['net_interest'] + \
				        alpha_mean * quarter['proprietors_income'] + \
				        quarter['rental_income'] - \
				        (quarter['housing_net_interest'] + \
				         alpha_mean*quarter['housing_proprietors_income'] \
				         + quarter['housing_rental_income'])) + \
		    quarter['gov_taxes_corp_income'] + \
		    quarter['real_estate_taxes_business']) / \
                    (quarter['net_operating_surplus_private'] - \
                     quarter['housing_net_operating_surplus'] - \
                     (1-alpha_mean)*(quarter['proprietors_income'] \
		                     - quarter['housing_proprietors_income']))  

### 2019-09-10: The following capital tax rate calculation is consistent with
### the measurement in Gomme and Rupert (2007)

quarter['tau_k_gr'] = (quarter['tau_h'] * (quarter['net_interest'] + \
				 alpha_mean * quarter['proprietors_income'] + \
				 quarter['rental_income'] - \
				 (quarter['housing_net_interest'] + \
				  alpha_mean * quarter['housing_proprietors_income'] \
				  + quarter['housing_rental_income'])) + \
		       quarter['gov_taxes_corp_income'] + \
		       quarter['real_estate_taxes_business']) / \
                       (quarter['net_operating_surplus_private'] \
                        - quarter['housing_net_operating_surplus'] \
                        + quarter['private_capital_consumption']\
                        - quarter['private_capital_consumption_housing']\
                        - (1-alpha_mean) * (quarter['proprietors_income'] - \
		                            quarter['housing_proprietors_income']))  

### 2016-08-15: Added calculation for consumption tax rate

quarter['consumption_taxes'] = quarter['state_local_sales_taxes'] + quarter['gov_excise_taxes'] + quarter['gov_custom_duties']

quarter['tau_c'] = quarter['consumption_taxes'] / \
  (quarter['PCE_nondurables'] \
   + quarter['PCE_services'] \
   + quarter['PCE_durables'] \
   + quarter['gov_consumption_expenditures'] \
   - quarter['consumption_taxes'])

annual['consumption_taxes'] = annual['state_local_sales_taxes'] + annual['gov_excise_taxes'] + annual['gov_custom_duties']
annual['tau_c'] = annual['consumption_taxes'] / \
  (annual['PCE_nondurables'] \
   + annual['PCE_services'] \
   + annual['PCE_durables'] \
   + annual['gov_consumption_expenditures'] \
   - annual['consumption_taxes'])

quarter = quarter.copy()

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

quarter['business_income_after_tax'] = quarter['business_income_pre_tax'] - \
    (quarter['tau_h'] * (quarter['net_interest'] + \
		         alpha_mean * quarter['proprietors_income'] + \
		         quarter['rental_income'] - \
		         (quarter['housing_net_interest'] + \
		          alpha_mean * quarter['housing_proprietors_income'] + \
		          quarter['housing_rental_income'])) + \
     quarter['gov_taxes_corp_income'] + \
     quarter['real_estate_taxes_business'])

quarter['all_capital_income_pre_tax'] = \
    quarter['net_operating_surplus_private'] - \
    (1 - alpha_mean) * quarter['proprietors_income']

quarter['all_capital_income_after_tax'] = \
    quarter['all_capital_income_pre_tax'] - \
    (quarter['tau_h'] * (quarter['net_interest'] + \
		         alpha_mean * quarter['proprietors_income'] + \
		         quarter['rental_income']) + \
     quarter['gov_taxes_corp_income'] + \
     quarter['real_estate_taxes_business'] + \
     quarter['property_taxes_household']) 

### The `usual' macroaggregates, this time expressed per capita.

quarter['real_pc_y'] = 1000*quarter['real_y'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_yp'] = 1000*quarter['real_yp'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_cm'] = 1000*quarter['real_cm'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_xs'] = 1000*quarter['real_xs'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_xe'] = 1000*quarter['real_xe'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_xh'] = 1000*quarter['real_xh'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_xd'] = 1000*quarter['real_xd'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_ks'] = 1000*quarter['real_ks'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_ke'] = 1000*quarter['real_ke'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_kh'] = 1000*quarter['real_kh'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['real_pc_kd'] = 1000*quarter['real_kd'] / quarter['civilian_noninstitutional_pop_16_plus']
quarter['pc_hours'] = 1000*quarter['hours'] / quarter['civilian_noninstitutional_pop_16_plus']

quarter['capital_gain'] = quarter['relative_price_investment'] / \
    quarter['relative_price_investment'].shift(1)
mean_capital_gain = quarter['capital_gain'].mean()

### Each return to capital series is computed pre- and after-tax. In the
### `basic' calculation, a `capital gain' is included in the return. This
### capital gain is the change in the relative price of investment goods (not
### from stock market data, or price of housing data). `no_gain' versions of
### the returns simply omit this capital gain component. The `constant_gain'
### versions use the mean capital gain in the place of the time-varying
### capital gain.

### Returns to business capital

quarter['return_business_capital_pre_tax'] =\
  100*((quarter['business_income_pre_tax']/4 /\
        (quarter['deflator_consumption']/100) /\
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1))\
        + quarter['capital_gain'])**4 - 1)

quarter['return_business_capital_after_tax'] = \
  100*((quarter['business_income_after_tax']/4 / \
        (quarter['deflator_consumption']/100) / \
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1))  \
        + quarter['capital_gain'])**4 - 1)

quarter['return_business_capital_pre_tax_no_gain'] =\
  100*((quarter['business_income_pre_tax']/4 /\
        (quarter['deflator_consumption']/100) /\
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1)) + 1)**4 - 1)

quarter['return_business_capital_after_tax_no_gain'] = \
  100*((quarter['business_income_after_tax']/4 / \
        (quarter['deflator_consumption']/100) / \
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1)) + 1)**4 - 1) 

quarter['return_business_capital_pre_tax_constant_gain'] =\
  100*((quarter['business_income_pre_tax']/4 /\
        (quarter['deflator_consumption']/100) /\
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1)) + mean_capital_gain)**4 - 1)

quarter['return_business_capital_after_tax_constant_gain'] = \
  100*((quarter['business_income_after_tax']/4 / \
        (quarter['deflator_consumption']/100) / \
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1)) + mean_capital_gain)**4 - 1) 

### Returns to all capital income (business and household)

quarter['return_all_capital_pre_tax'] =\
  100*((quarter['all_capital_income_pre_tax']/4 /\
        (quarter['deflator_consumption']/100) /\
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1) + quarter['real_kh'].shift(1))\
        + quarter['capital_gain'])**4 - 1)

quarter['return_all_capital_after_tax'] = \
  100*((quarter['all_capital_income_after_tax']/4 / \
        (quarter['deflator_consumption']/100) / \
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)
         + quarter['real_ke'].shift(1) + quarter['real_kh'].shift(1))\
        + quarter['capital_gain'])**4 - 1) 

quarter['return_all_capital_pre_tax_no_gain'] =\
  100*((quarter['all_capital_income_pre_tax']/4 /\
        (quarter['deflator_consumption']/100) /\
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1) + quarter['real_kh'].shift(1)) + 1)**4\
        - 1)

quarter['return_all_capital_after_tax_no_gain'] = \
  100*((quarter['all_capital_income_after_tax']/4 / \
        (quarter['deflator_consumption']/100) / \
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1) + quarter['real_kh'].shift(1)) + 1)**4 - 1) 

quarter['return_all_capital_pre_tax_constant_gain'] =\
  100*((quarter['all_capital_income_pre_tax']/4 /\
        (quarter['deflator_consumption']/100) /\
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1) + quarter['real_kh'].shift(1)) + mean_capital_gain)**4\
        - 1)

quarter['return_all_capital_after_tax_constant_gain'] = \
  100*((quarter['all_capital_income_after_tax']/4 / \
        (quarter['deflator_consumption']/100) / \
        (quarter['real_inventories'].shift(1)\
         + quarter['real_ks'].shift(1)\
         + quarter['real_ke'].shift(1) + quarter['real_kh'].shift(1)) + mean_capital_gain)**4 - 1) 

### Returns to housing income

quarter['return_housing_capital_pre_tax'] =\
  100*(((quarter['all_capital_income_pre_tax'] - quarter['business_income_pre_tax'])/4 /\
        (quarter['deflator_consumption']/100) /\
        quarter['real_kh'].shift(1) + quarter['capital_gain'])**4 - 1)

quarter['return_housing_capital_after_tax'] = \
  100*(((quarter['all_capital_income_after_tax'] - quarter['business_income_after_tax'])/4 / \
        (quarter['deflator_consumption']/100) / quarter['real_kh'].shift(1) + quarter['capital_gain'])**4 - 1) 

quarter['return_housing_capital_pre_tax_no_gain'] =\
  100*(((quarter['all_capital_income_pre_tax'] - quarter['business_income_pre_tax'])/4 /\
        (quarter['deflator_consumption']/100) / quarter['real_kh'].shift(1) + 1)**4 - 1)

quarter['return_housing_capital_after_tax_no_gain'] = \
  100*(((quarter['all_capital_income_after_tax'] - quarter['business_income_after_tax'])/4 / \
        (quarter['deflator_consumption']/100) / quarter['real_kh'].shift(1) + 1)**4 - 1) 

quarter['return_housing_capital_pre_tax_constant_gain'] =\
  100*(((quarter['all_capital_income_pre_tax'] - quarter['business_income_pre_tax'])/4 /\
        (quarter['deflator_consumption']/100) / quarter['real_kh'].shift(1) + mean_capital_gain)**4 - 1)

quarter['return_housing_capital_after_tax_constant_gain'] = \
  100*(((quarter['all_capital_income_after_tax'] - quarter['business_income_after_tax'])/4 / \
        (quarter['deflator_consumption']/100) / quarter['real_kh'].shift(1) + mean_capital_gain)**4 - 1) 

for i in ['return_business_capital_pre_tax',
          'return_business_capital_after_tax',
          'return_business_capital_pre_tax_no_gain',
          'return_business_capital_after_tax_no_gain',
          'return_business_capital_pre_tax_constant_gain',
          'return_business_capital_after_tax_constant_gain',
          'return_all_capital_pre_tax',
          'return_all_capital_after_tax',
          'return_all_capital_pre_tax_no_gain',
          'return_all_capital_after_tax_no_gain',
          'return_all_capital_pre_tax_constant_gain',
          'return_all_capital_after_tax_constant_gain',
          'return_housing_capital_pre_tax',
          'return_housing_capital_after_tax',
          'return_housing_capital_pre_tax_no_gain',
          'return_housing_capital_after_tax_no_gain',
          'return_housing_capital_pre_tax_constant_gain',
          'return_housing_capital_after_tax_constant_gain']:
    globals()[i] = quarter[i].loc[tKW:].mean()

### The Solow residual (total factor productivity)

quarter['solow_residual'] = (quarter['real_y'] / (quarter['real_ke'].shift(1) + quarter['real_ks'].shift(1))**alpha_mean)**(1/(1-alpha_mean)) / quarter['hours']

### Aggregated investment series

quarter['real_pc_xall'] = quarter['real_pc_xs'] + quarter['real_pc_xe'] + quarter['real_pc_xh'] + quarter['real_pc_xd']
quarter['real_pc_xmarket'] = quarter['real_pc_xs'] + quarter['real_pc_xe']
quarter['real_pc_xhome'] = quarter['real_pc_xh'] + quarter['real_pc_xd']
quarter['real_pc_kall'] = quarter['real_pc_ks'] + quarter['real_pc_ke'] + quarter['real_pc_kh'] + quarter['real_pc_kd']
quarter['real_pc_kmarket'] = quarter['real_pc_ks'] + quarter['real_pc_ke']
quarter['real_pc_khome'] = quarter['real_pc_kh'] + quarter['real_pc_kd']
quarter['productivity'] = quarter['real_pc_y'] / quarter['pc_hours']

quarter = quarter.copy()

### Collection of data that is Hodrick-Prescott filtered

print(" Preparing to HP filter ")

#### Data for calibration paper
#
#hpname = ['real_pc_y', \
#	 'real_pc_cm', \
#	 'real_pc_xall',  \
#	 'real_pc_xmarket', \
#	 'real_pc_xhome',  \
#	 'pc_hours', \
#	 'productivity', \
#	 'real_pc_kall', \
#	 'real_pc_kmarket', \
#	 'real_pc_khome', \
#	 'solow_residual', \
#	 'relative_price_investment']
#
#d = []
#for s in hpname:
#    d.append(quarter[s].last_valid_index().strftime('%Y-%m-%d'))
#
#dateend = min(d)
#datestart = '1954-01-01'
#
#filtereddata = pd.DataFrame()
#for s in hpname:
#    hpcycle, hptrend = hpfilter(np.log(quarter.loc[datestart: dateend, s]), lamb=1600)
#    filtereddata[s] = hpcycle
#
#filtereddata.to_csv('calibration-hpfiltered.csv')

### Data including real returns

###quarter = quarter.loc[quarter.index < '2020-01-01'] ## For calibration paper

hpname = ['real_pc_y', \
	 'real_pc_cm', \
	 'real_pc_xall',  \
	 'real_pc_xmarket', \
	 'real_pc_xhome',  \
	 'pc_hours', \
	 'productivity', \
	 'real_pc_kall', \
	 'real_pc_kmarket', \
	 'real_pc_khome', \
	 'solow_residual', \
	 'relative_price_investment', \
	 'tau_k', \
	 'tau_n']

pctname = ['return_business_capital_pre_tax', \
         'return_business_capital_pre_tax_no_gain', \
         'return_business_capital_after_tax', \
         'return_business_capital_after_tax_no_gain', \
         'return_all_capital_pre_tax', \
         'return_all_capital_pre_tax_no_gain', \
         'return_all_capital_after_tax', \
         'return_all_capital_after_tax_no_gain', \
         'return_housing_capital_pre_tax', \
         'return_housing_capital_pre_tax_no_gain', \
         'return_housing_capital_after_tax', \
         'return_housing_capital_after_tax_no_gain']

d = []
for s in hpname + pctname:
    d.append(quarter[s].last_valid_index().strftime('%Y-%m-%d'))

dateend = min(d)
datestart = '1954-01-01'

filtereddata = pd.DataFrame()
for s in hpname:
    hpcycle, hptrend = hpfilter(np.log(quarter.loc[datestart: dateend, s]), lamb=1600)
    filtereddata[s] = hpcycle

for s in pctname:
    smean = quarter.loc[datestart: dateend, s].mean()
    filtereddata[s] = (quarter.loc[datestart: dateend, s] - smean) / smean

sname = hpname + pctname

filtereddata.to_csv('hpfiltered.csv')

T, N = filtereddata.shape

print(" Done HP filtering ")

### Produce a table of second moments

texfile = 'us-moments.tex'

#if os.path.exists(texfile):
#    os.remove(texfile)

usstring = ['Output', \
            'Consumption', \
            'Investment', \
            'Investment: market', \
	    'Investment: home', \
            'Hours', 
            'Productivity', \
            'Capital',  \
            'Capital: market', \
            'Capital: home', \
	    'Solow Residual', \
	    'Price of Investment', \
            'Capital tax', \
            'Labor tax', \
	    'Business capital, pre-tax, capital gain', \
	    'Business capital, pre-tax, no capital gain', \
	    'Business capital, after-tax, capital gain', \
	    'Business capital, after-tax, no capital gain', \
	    'All capital, pre-tax, capital gain', \
	    'All capital, pre-tax, no capital gain', \
	    'All capital, after-tax, capital gain', \
	    'All capital, after-tax, no capital gain', \
	    'Housing capital, pre-tax, capital gain', \
	    'Housing capital, pre-tax, no capital gain', \
	    'Housing capital, after-tax, capital gain', \
	    'Housing capital, after-tax, no capital gain']

lines = ['\\documentclass[12pt]{article}\n', \
         '\\usepackage{graphicx,siunitx,rotating,multirow,times,mathptmx,booktabs}\n', \
         '\\usepackage[margin=1in]{geometry}\n', \
         '\\sisetup{group-separator={},round-mode=places,table-format=2.2}\n', \
         '\\begin{document}\n', \
         '\\begin{sidewaystable}\n', \
         '\\begin{center}\n', \
         '\\caption{U.S.\\@ ' + datestart + '--' + dateend + ': Selected Moments}\n', \
         '\\label{tab:rbc-lead-lag}\n', \
         '\\begin{tabular}{l S S S S S S S S S S}\n', \
         '\\toprule\n', \
         '\t\t& {\\multirow{2}*{\\parbox[t]{1in}{\\centering Standard Deviation}}}\n', \
         '\t\t& \\multicolumn{9}{c}{Cross Correlation of Real Output With}\n', \
         '\\\\ \\cmidrule{3-11} \n', \
         '\t\t&\n', \
         '\t\t& {$x_{t-4}$}\n', \
         '\t\t& {$x_{t-3}$}\n', \
         '\t\t& {$x_{t-2}$}\n', \
         '\t\t& {$x_{t-1}$}\n', \
         '\t\t& {$x_t$}\n', \
         '\t\t& {$x_{t+1}$}\n', \
         '\t\t& {$x_{t+2}$}\n', \
         '\t\t& {$x_{t+3}$}\n', \
         '\t\t& {$x_{t+4}$}\n', \
         '\\\\\n', \
         '\\midrule\n']

ussd = 100 * filtereddata.std()

target = 'real_pc_y'

lead_lag_corr = pd.DataFrame.from_dict(
    {x: [filtereddata[target].corr(filtereddata[x].shift(-t)) for t in range(-4,5)] for x in filtereddata.columns})

autocorr = pd.DataFrame.from_dict({x: [filtereddata[x].autocorr()] for x in filtereddata.columns})

for s, x in zip(sname, usstring):
    newline = x + ' & ' + str(ussd[s])
    for i in range(9):
        newline = newline + ' & ' + str(lead_lag_corr[s][i])
    newline = newline + '\\\\\n'
    lines.append(newline)

lines.append('\\bottomrule\n')
lines.append('\\end{tabular}\n')
lines.append('\\end{center}\n')
lines.append('\\end{sidewaystable}\n')
lines.append('\\newpage\n')

lines.append('\\begin{table}[htbp]\n')
lines.append('\\centering  \\vskip -1cm\\scriptsize\n')
lines.append('\\caption{U.S.\\@ Return to Capital and Tax Rate on Household Income}\n')
lines.append('\\label{tab:return}\n')
lines.append('\\begin{tabular}{c S S S S c S S S S} \n')
lines.append('\\toprule\n')
lines.append('& \\multicolumn{4}{c}{Return to Capital} && \\multicolumn{4}{c}{Tax Rate, $\\tau_h$} \\\\\n')
lines.append('\\cmidrule{2-5}  \\cmidrule{7-10}\n')
lines.append('Year & {Q1}  & {Q2} & {Q3} & {Q4} && {Q1} & {Q2} & {Q3} & {Q4}   \\\\\n')
lines.append('\\midrule\n')

Rk = copy.copy(quarter['return_business_capital_after_tax']).dropna(inplace=False).to_numpy()
tau_h = copy.copy(quarter['tau_h']).dropna(inplace=False).to_numpy()
tau_n = copy.copy(quarter['tau_n']).dropna(inplace=False).to_numpy()
tau_k = copy.copy(quarter['tau_k']).dropna(inplace=False).to_numpy()

N = math.ceil(len(Rk)/4)

t = 0
for yr in range(N):
    newline = str(yr+1954)
    for i in range(4):
        if t + i < len(Rk):
            newline = newline + ' & ' + str(Rk[t+i])
    newline = newline + '&'
    for i in range(4):
        if t + i < len(tau_h):
            newline = newline + ' & ' + str(100 * tau_h[t+i])
    newline = newline + '\\\\\n'
    lines.append(newline)
    t = t+4

lines.append('\\bottomrule\n')
lines.append('\\thispagestyle{empty}\n')
lines.append('\\end{tabular}\n')
lines.append('\\end{table}\n')

### Write out the tax rates on capital and labor income

lines.append('\\begin{table}[htbp]\n')
lines.append('\\centering  \\vskip -1cm\\scriptsize\n')
lines.append('\\caption{U.S.\\@ Tax Rates on Labor and Capital Income}\n')
lines.append('\\label{tab:return}\n')
lines.append('\\begin{tabular}{c S S S S c S S S S} \n')
lines.append('\\toprule\n')
lines.append('& \\multicolumn{4}{c}{Tax Rate, $\\tau_n$} && \\multicolumn{4}{c}{Tax Rate, $\\tau_k$} \\\\\n')
lines.append('\\cmidrule{2-5}  \\cmidrule{7-10}\n')
lines.append('Year & {Q1}  & {Q2} & {Q3} & {Q4} && {Q1}  & {Q2} & {Q3} & {Q4}   \\\\\n')
lines.append('\\midrule\n')

t = 0
for yr in range(N):
    newline = str(yr+1954)
    for i in range(4):
        if t + i < len(tau_n):
            newline = newline + ' & ' + str(100 * tau_n[t+i])
    newline = newline + '&'
    for i in range(4):
        if t + i < len(tau_k):
            newline = newline + ' & ' + str(100 * tau_k[t+i])
    newline = newline + '\\\\\n'
    lines.append(newline)
    t = t+4

lines.append('\\bottomrule\n')
lines.append('\\thispagestyle{empty}\n')
lines.append('\\end{tabular}\n')
lines.append('\\end{table}\n')
lines.append('\\end{document}\n')

with open(texfile, 'w') as f:
         f.writelines(lines)

rawdata = quarter.loc['1947-01-01':, hpname + pctname]

rawdata.to_csv('usdata.csv')
rawdata.to_csv('usdata.dat', sep=' ')

annual['alpha'].dropna(inplace=False).to_csv('alpha.dat', sep='\t')

quarter.loc['1947-01-01':, ['relative_price_investment', 'solow_residual', 'tau_n', 'tau_k']].to_csv('shocks.csv')

### Immediately after the Korean war
firstdate = '1954-01-01'
lastdate = quarter['solow_residual'].last_valid_index().strftime('%Y-%m-%d')

N = len(quarter.loc[firstdate:lastdate, 'solow_residual'].dropna())

quarter.loc[firstdate:lastdate, 'trend'] = np.arange(1, N+1, 1)
quarter['temp'] = np.log(quarter['solow_residual'])
quarter['log_solow_residual'] = quarter.loc[firstdate:, 'temp']
quarter['temp'] = quarter['temp'].shift(1)
quarter['lag_log_solow_residual'] = quarter.loc[firstdate:, 'temp']

###lastdate = '2019-10-01'

Solow_result = sm.ols(formula = "log_solow_residual ~ trend + lag_log_solow_residual",
                      data = quarter.loc[firstdate:lastdate]).fit()

print(Solow_result.summary())
print('SD of innovation:', Solow_result.scale**0.5)

Solow_result.resid.to_csv('solow-residual.dat', header=None, sep='\t')

#############################################################################
tau_k = quarter['tau_k'].loc[tKW:].mean()
tau_n = quarter['tau_n'].loc[tKW:].mean()
tau_h = quarter['tau_h'].loc[tKW:].mean()
tau_c = quarter['tau_c'].loc[tKW:].mean()

data = [['Capital\'s share of income', alpha_mean],
        ['Depreciation rate', delta_all_annual],
        ['\\quad  Market', delta_market_annual],
        ['\\qquad Structures', delta_market_structures_annual],
        ['\\qquad Equipment \\& Software', delta_market_equipment_annual],
        ['\\quad  Home', delta_home_annual],
        ['\\qquad Housing', delta_home_structures_annual],
        ['\\qquad Durables', delta_home_durables_annual],
        ['Investment-output', x_y],
        ['\\quad  Market', xm_y],
        ['\\qquad Structures', xms_y],
        ['\\qquad Equipment \\& Software', xme_y],
        ['\\quad  Home', xh_y],
        ['\\qquad Housing', xhh_y],
        ['\\qquad Durables', xhd_y],
        ['Capital-output', k_y],
        ['\\quad  Market', km_y],
        ['\\qquad Structures', kms_y],
        ['\\qquad Equipment \\& Software', kme_y],
        ['\\quad  Home', kh_y],
        ['\\qquad Housing', khh_y],
        ['\\qquad Durables', khd_y],
        ['Return to capital', ''],
        ['\\quad All capital', ''],
        ['\\qquad Pre-tax', return_all_capital_pre_tax],
        ['\\qquad After-tax', return_all_capital_after_tax],
        ['\\qquad Pre-tax, constant gain', return_all_capital_pre_tax_constant_gain],
        ['\\qquad After-tax, constant gain', return_all_capital_after_tax_constant_gain],
        ['\\qquad Pre-tax, no gain', return_all_capital_pre_tax_no_gain],
        ['\\qquad After-tax, no gain', return_all_capital_after_tax_no_gain],
        ['\\quad Business capital', ''],
        ['\\qquad Pre-tax', return_business_capital_pre_tax],
        ['\\qquad After-tax', return_business_capital_after_tax],
        ['\\qquad Pre-tax, constant gain', return_business_capital_pre_tax_constant_gain],
        ['\\qquad After-tax, constant gain', return_business_capital_after_tax_constant_gain],
        ['\\qquad Pre-tax, no gain', return_business_capital_pre_tax_no_gain],
        ['\\qquad After-tax, no gain', return_business_capital_after_tax_no_gain],
        ['\\quad Housing capital', ''],
        ['\\qquad Pre-tax', return_housing_capital_pre_tax],
        ['\\qquad After-tax', return_housing_capital_after_tax],
        ['\\qquad Pre-tax, constant gain', return_housing_capital_pre_tax_constant_gain],
        ['\\qquad After-tax, constant gain', return_housing_capital_after_tax_constant_gain],
        ['\\qquad Pre-tax, no gain', return_housing_capital_pre_tax_no_gain],
        ['\\qquad After-tax, no gain', return_housing_capital_after_tax_no_gain],
        ['\\tau_h', tau_h],
        ['\\tau_n', tau_n],
        ['\\tau_k', tau_k],
        ['\\tau_c', tau_c],
        ['Technology shock',''],
        ['\\quad Autoregressive parameter', Solow_result.params.iloc[2]],
        ['\\quad Standard deviation of the residual', Solow_result.scale**0.5]]

calibration = pd.DataFrame(data, columns=['Description', 'Value'])

open('calibration.txt', 'w').write(tabulate(calibration, headers='keys', tablefmt='latex_raw', showindex=False))

#############################################################################

fig, ax = plt.subplots()
ax.plot(1-annual['alpha'], clip_on = False)
ax.set_title('US Labor Share of Income, ' + annual['alpha'].first_valid_index().strftime('%Y') + '-' + annual['alpha'].last_valid_index().strftime('%Y') + ' (mean = ' + str(round(1-alpha_mean, 3)) + ')')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('labor-share.pdf')
fig.savefig('labor-share.png')
fig.savefig('labor-share.jpeg')
plt.close()

fig, ax = plt.subplots()
ax.plot(100*(1-annual['alpha'])/(1-annual['alpha']['2012-01-01']), label='Gomme-Rupert', clip_on = False)
ax.plot(100*annual['LABSHPUSA156NRUG']/annual['LABSHPUSA156NRUG']['2012-01-01'], label='Feenstra et al.', clip_on = False)
ax.plot(quarter['labor share non-farm business'], label='Non-farm business sector', clip_on = False)
ax.plot(quarter['labor share business'], label='Business sector', clip_on = False)
ax.set_title('US Labor Share of Income, 2012=10')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
ax.legend(frameon=False)
fig.savefig('labor-share-index.pdf')
fig.savefig('labor-share-index.png')
fig.savefig('labor-share-index.jpeg')
plt.close()


fig, ax = plt.subplots()
ax.plot(100-100*annual['alpha'], label='Gomme-Rupert', clip_on = False)
ax.plot(100*annual['LABSHPUSA156NRUG'], label='Feenstra et al.', clip_on = False)
ax.set_title('US Labor Share of Income')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
ax.legend(frameon=False)
fig.savefig('labor-share-compared.pdf')
fig.savefig('labor-share-compared.png')
fig.savefig('labor-share-compared.jpeg')
plt.close()

my_title = 'Gomme, Ravikumar and Rupert (2011, Updated):\n'

fig, ax = plt.subplots()
ax.plot(quarter['return_business_capital_pre_tax_constant_gain'], label='Business (pre-tax)', clip_on = False)
ax.plot(quarter['return_all_capital_pre_tax_constant_gain'], label='All (pre-tax)', clip_on = False)
ax.plot(quarter['return_business_capital_after_tax_constant_gain'], label='Business (after-tax)', clip_on = False)
ax.plot(quarter['return_all_capital_after_tax_constant_gain'], label='All (after-tax)', clip_on = False)
ax.legend(frameon=False)
ax.set_title(my_title + 'Real Returns on Capital (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('return_to_capital.png')
fig.savefig('return_to_capital.jpg')
fig.savefig('return_to_capital.pdf')
plt.close()

fig, ax = plt.subplots()
ax.plot(quarter['return_business_capital_pre_tax_constant_gain'], label='Pre-tax', clip_on = False)
ax.plot(quarter['return_business_capital_after_tax_constant_gain'], label='After-tax', clip_on = False)
ax.legend(frameon=False)
ax.set_title(my_title + 'Real Returns on Business Capital (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('return_to_business_capital.png')
fig.savefig('return_to_business_capital.jpg')
fig.savefig('return_to_business_capital.pdf')
plt.close()

fig, ax = plt.subplots()
ax.plot(quarter['return_all_capital_pre_tax_constant_gain'], label='Pre-tax', clip_on = False)
ax.plot(quarter['return_all_capital_after_tax_constant_gain'], label='`After-tax', clip_on = False)
ax.legend(frameon=False)
ax.set_title(my_title + 'Real Returns on All Capital (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('return_to_all_capital.png')
fig.savefig('return_to_all_capital.jpg')
fig.savefig('return_to_all_capital.pdf')
plt.close()

fig, ax = plt.subplots()
ax.plot(quarter['return_housing_capital_pre_tax_constant_gain'], label='All (pre-tax)', clip_on = False)
ax.plot(quarter['return_housing_capital_after_tax_constant_gain'], label='All (after-tax)', clip_on = False)
ax.legend(frameon=False)
ax.set_title(my_title + 'Real Returns on Housing Capital (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('return_to_housing_capital.png')
fig.savefig('return_to_housing_capital.jpg')
fig.savefig('return_to_housing_capital.pdf')
plt.close()

fig, ax = plt.subplots()
ax.plot(quarter['return_business_capital_pre_tax_constant_gain'], label='Business', clip_on = False)
ax.plot(quarter['return_all_capital_pre_tax_constant_gain'], label='All', clip_on = False)
ax.plot(quarter['return_housing_capital_pre_tax_constant_gain'], label='Housing', clip_on = False)
ax.legend(frameon=False)
ax.set_title(my_title + 'Pre-tax Real Returns on Capital (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('pre_tax_return_to_capital.png')
fig.savefig('pre_tax_return_to_capital.jpg')
fig.savefig('pre_tax_return_to_capital.pdf')
plt.close()


fig, ax = plt.subplots()
ax.plot(quarter['return_business_capital_after_tax_constant_gain'], label='Business', clip_on = False)
ax.plot(quarter['return_all_capital_after_tax_constant_gain'], label='All', clip_on = False)
ax.plot(quarter['return_housing_capital_after_tax_constant_gain'], label='Housing', clip_on = False)
ax.legend(frameon=False)
ax.set_title(my_title + 'After-tax Real Returns on Capital (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('after_tax_return_to_capital.png')
fig.savefig('after_tax_return_to_capital.jpg')
fig.savefig('after_tax_return_to_capital.pdf')
plt.close()


fig, ax = plt.subplots()
ax.plot(quarter['tau_k'], clip_on = False)
ax.set_title(my_title + 'Capital Income Tax Rate (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('tax_capital.png')
fig.savefig('tax_capital.jpg')
fig.savefig('tax_capital.pdf')
plt.close()



fig, ax = plt.subplots()
ax.plot(quarter['tau_n'], clip_on = False)
ax.set_title(my_title + 'Labor Incomed Tax Rate (percent)')
#plt.axhline(y = 0.0, color = 'grey', linestyle = '-')
fig.savefig('tax_labor.png')
fig.savefig('tax_labor.jpg')
fig.savefig('tax_labor.pdf')
plt.close()

