#rr_clean.py
#last updated 6. march 2015

import pandas as pd
import matplotlib.pyplot as plt
import sys
import seaborn as sn
import Quandl
import numpy as np
import statsmodels as sm

#download, merge and clean dataset

brentprices=pd.read_excel("http://www.eia.gov/dnav/pet/hist_xls/RBRTEm.xls", sheetname="Data 1")
brentprices.columns=["date", "oil_price_nom"]
brentprices=brentprices[2:]
brentprices["date"]=pd.to_datetime(brentprices["date"])
brentprices=brentprices.set_index("date")
brentprices["oil_price_nom"]=[float(i) for i in brentprices["oil_price_nom"]]
oil


#from bp via Quandl

oil_long = Quandl.get("BP/CRUDE_OIL_PRICES", authtoken="WHFoD2EKU2J8H8yGn_bS")
oil_long.columns=["oil_price_nom", "oil_price_real"]
oil_long["year"]=oil_long.index.year

oil_nom=oil_long.reset_index()
oil_nom=oil_nom[["year", "oil_price_nom"]]
oil_nom=oil_nom[oil_nom.year<1987].append(brent_yearly.reset_index())
oil_nom=oil_nom.set_index("year")

#real - deflate
#investment deflator
deflator=pd.read_table('http://research.stlouisfed.org/fred2/data/USAGDPDEFAISMEI.txt', skiprows=16, sep=" ")
deflator.iloc[-5:,2]=deflator.iloc[-5:,1]
deflator=deflator[[0,2]]
deflator.columns=["year", "deflator_2010"]
deflator["year"]=pd.to_datetime(deflator["year"])
deflator=deflator.set_index("year")
deflator.index=deflator.index.year

oil_price=oil_nom.merge(deflator, how="inner", left_index=True, right_index=True)
oil_price["oil_price_real"]=oil_price["oil_price_nom"]*(100/oil_price["deflator_2010"])
oil_price=oil_price.reset_index()
oil_price.columns=["year", "oil_price_nom", "deflator_2010", "oil_price_real"]
oil_price.to_csv("research/oil_prices/data/oil_price.csv", index=False)
#from npd
overview=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
month_prod=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_production_monthly&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.56&CultureCode=nb-no")
year_prod=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_production_yearly&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=62.73.195.218&CultureCode=en")
tot_month_prod=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_production_totalt_NCS_month__DisplayAllRows&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.56&CultureCode=nb-no")
#in MillSm3

#licensee=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_licensee_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
#owners=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_owner_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
#operators=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_operator_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
status=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_activity_status_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
yearly_investments=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_investment_yearly&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
#descriptions=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_description&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
reserves=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_reserves&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en")
in_place_oil=pd.read_csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_in_place_volumes&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=62.73.195.218&CultureCode=en")

in_place_oil=in_place_oil[['﻿fldName', 'fldInplaceOil']]
in_place_oil.columns=["name", "in_place_oil_mill_sm3"]

reserves=reserves[['﻿fldName', 'fldRecoverableOil']]
reserves.columns=["name", "recoverable_oil"]

pdo_approved=status[status["fldStatus"]=="PDO APPROVED"]
pdo_approved=pdo_approved[['﻿fldName', 'fldStatusFromDate', 'fldStatusToDate']]
pdo_approved.columns = ["name", "pdo_approved_date", "producing_date"]


shutdown=status[status["fldStatus"]=="SHUT DOWN"]
shutdown=shutdown[['﻿fldName', 'fldStatusFromDate']]
shutdown.columns=["name", "shutdown_date"]
shutdown=shutdown.drop_duplicates(subset="name")

#yearly_investments
yearly_investments=yearly_investments[['﻿prfInformationCarrier', 'prfYear', 'prfInvestmentsMillNOK']]
yearly_investments.columns=["name", "year", "nom_invest_mill_nok"]
deflator=deflator.reset_index()
deflator.columns=["year", "deflator_2010"]
yearly_investments=yearly_investments.merge(deflator, on="year")
yearly_investments["real_invest_mill_nok_2010"] = yearly_investments["nom_invest_mill_nok"]*(100/yearly_investments["deflator_2010"])
del yearly_investments["deflator_2010"]

#geography - extract approximate latt/long
geography=pd.read_table("research/oil_prices/data/fldArea.csv", sep=",")

geography=open("research/oil_prices/data/fldArea.csv")
geography.readline()

#now put together minimalistic data set
#real yearly oil price, in place oil, recoverable reserves
#
oil_prod=year_prod[['﻿prfInformationCarrier',"prfYear",  'prfPrdOilNetMillSm3']]
oil_prod.columns=["name","year","oil_prod_mill_sm3"]

#Create two data sets - production level (year-field) and field level.

oil_price=oil_price.reset_index()


#field level data
field_data = in_place_oil.merge(reserves, on="name")
field_data=field_data.merge(pdo_approved, on="name")
field_data=field_data.merge(shutdown, on="name", how="outer")

field_data["shutdown"] = pd.notnull(field_data["shutdown_date"])

field_data.to_csv('/Users/johannesmauritzen/research/oil_prices/data/field_data.csv')

#production data
del oil_price["oil_price_nom"]
del yearly_investments["nom_invest_mill_nok"]

#add lags

oil_price=pd.concat([oil_price, oil_price["oil_price_real"].shift(), 
	oil_price["oil_price_real"].shift(2),
	oil_price["oil_price_real"].shift(3),
	oil_price["oil_price_real"].shift(4),
	oil_price["oil_price_real"].shift(5),
	oil_price["oil_price_real"].shift(6),
	oil_price["oil_price_real"].shift(7),
	oil_price["oil_price_real"].shift(8)], axis=1)

oil_price.columns=["year", "deflator", "price", "price_l1", 
	"price_l2", "price_l3", "price_l4", "price_l5", "price_l6", "price_l7", "price_l8"]

prod_data=oil_prod.merge(oil_price, on="year")
prod_data=prod_data.merge(yearly_investments, on=["name", "year"])
prod_data=prod_data.merge(field_data, on="name")

#add data on cost
#prod_data=pd.read_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')
#from data cleaned from industry_cost.py
cost_index=pd.read_csv("research/oil_prices/data/cost_index.csv")
prod_data=prod_data.merge(cost_index[["year", "cost_index"]], on="year", how="left")

prod_data=prod_data.sort(columns=["name", "year"])
#add production year data
def create_prod_year(year):
	return(pd.DataFrame({"prod_year":[i for i in range(len(year))]}))

prod_data['prod_year']=prod_data.groupby("name")["year"].transform(create_prod_year)

prod_data.to_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv', index=False)


#add build-out dummy
prod_data=pd.read_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

def build_out_phase(field):
	"""
	returns dummy variables with input of fields
	"""
	#test
	#field=prod_data[prod_data.name=="STATFJORD"]
	#
	prod_max=field.oil_prod_mill_sm3.max()
	year_max=field.year[field.oil_prod_mill_sm3==prod_max].max()
	field["build_out"]=0
	field["build_out"][field.year<year_max]=1
	return(field.build_out)

build_out=prod_data.groupby("name").apply(build_out_phase).reset_index()

prod_data["build_out"]=build_out["build_out"]

prod_data.to_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv', index=False)

# from world bank - world GDP
prod_data = pd.read_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

world_gdp = Quandl.get("WORLDBANK/WLD_NY_GDP_MKTP_KD_ZG", authtoken="WHFoD2EKU2J8H8yGn_bS")

world_gdp['year'] = world_gdp.index.year

world_gdp.reset_index(inplace=True)

del world_gdp["Date"]
world_gdp.columns = ["world_gdp", "year"]
prod_data = prod_data.merge(world_gdp, how='left', on="year")

prod_data.to_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv', index=False)



