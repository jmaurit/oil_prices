#costs.py

import pandas as pd
import matplotlib.pyplot as plt
import sys
import seaborn as sn
import Quandl
import numpy as np
import statsmodels as sm

#producer price index:ssb, norway 2000 dollars
ppi=pd.read_csv("http://data.ssb.no/api/v0/dataset/27263.csv?lang=en")
ppi_oandg=ppi[ppi["industry/commodity group"]=="SNN06 Extraction of oil and natural gas"]
ppi_oandg=ppi_oandg[ppi_oandg["market"]=="1 Domestic and export market, total"]
ppi_oandg=ppi_oandg[ppi_oandg["contents"]=="Producer price index (2000=100)"]
ppi_oandg=ppi_oandg[["time","Producer price index. Oil/ gas extraction, manufacturing, mining and electricity by market, industry/commodity group, time and contents"]]
ppi_oandg.columns=["period", "ppi_oandg"]

year=[]
month=[]
for i in ppi_oandg["period"]:
	year.append(int(i[:4]))
	month.append(int(i[5:]))

ppi_oandg["year"]=year
ppi_oandg["month"]=month

#convert to float
ppi_oandg["ppi_oandg"]=[float(i) for i in ppi_oandg["ppi_oandg"]]
ppi_yearly=ppi_oandg[["year", "ppi_oandg"]].groupby("year").mean()

#merge with prices
ppi_yearly=oil_price.merge(ppi_yearly.reset_index(), on="year", how="outer")

#info from eia
#Costs of Crude Oil and Natural Gas Wells Drilled
#All (Real*) Thousand dollars per well
#http://www.eia.gov/dnav/pet/pet_crd_wellcost_s1_a.htm

well_costs=pd.read_csv("research/oil_prices/data/well_costs.csv", sep=";", decimal=",")
well_costs["cost_index"]=well_costs["cost"]/754.6*100

cost_index=well_costs.merge(ppi_yearly.reset_index(), on="year", how="outer")
cost_index["ppi_oandg"][cost_index["ppi_oandg"].isnull()]=cost_index["cost_index"][cost_index["ppi_oandg"].isnull()]

cost_index=cost_index[["year", "cost_index"]]
cost_index.to_csv("research/oil_prices/data/cost_index.csv")

sm.tsa.arima_model.ARIMA(endog, order, exog=None, dates=None, freq=None, missing='none')

