#prep.py
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt

fields=pd.read_csv("/Users/johannesmauritzen/Google Drive/Research/rOil/data/oil_fields.csv")

del fields["Unnamed: 0"]

#interpret at 10$ change in oil price
fields["oil_price_real"]=fields["oil_price_real"]/10
fields["oil_price_real_l1"]=fields["oil_price_real_l1"]/10
fields["oil_price_real_l2"]=fields["oil_price_real_l2"]/10
fields["oil_price_real_l3"]=fields["oil_price_real_l3"]/10
fields["oil_price_real_l4"]=fields["oil_price_real_l4"]/10
fields["oil_price_real_l5"]=fields["oil_price_real_l5"]/10
fields["oil_price_real_l6"]=fields["oil_price_real_l6"]/10
fields["oil_price_real_l7"]=fields["oil_price_real_l7"]/10
fields["oil_price_real_l8"]=fields["oil_price_real_l8"]/10

fields["diff_oil_price"]=fields["diff_oil_price"]/10

#get rid of fields where tot. production is zero
fields=fields[fields["tot.prod"]>0]

#datetime format
fields["producing_from"]=pd.to_datetime(fields["producing_from"], format="%Y-%m-%d")
fields["producing_to"]=pd.to_datetime(fields["producing_to"], format="%Y-%m-%d")
fields["year"]=pd.to_datetime(fields["year"], format="%Y")

#drop duplicates
fields=fields.drop_duplicates(subset=["name", "year"])

#set name and year as multi-index

#finds max year of data frame
def max_year(df):
	#test
	#df=fields
	#test
	peak_prod=df.year_prod.max()
	peak_year=df.year[df.year_prod==peak_prod]
	return peak_year

fields=fields.reset_index()

maxyear=pd.DataFrame(fields.groupby("name").apply(max_year))
maxyear=maxyear.reset_index()
del maxyear["level_1"]
maxyear.columns=["name", "maxyear"]
#fields=fields.set_index(["name", "year"])

fields=fields.merge(maxyear, on="name", how="left")

#get rid of NAs 
fields=fields[fields["year_prod"].notnull()]

#change year to int:
years= [int(str(year)[:4]) for year in fields["year"]]
fields["year"]=years
	
	print year
year = str(fields["year"]).split()

#get producing time (in days)
init_year=fields["year"].groupby(fields["name"]).transform(min)
fields["init_year"]=init_year

fields["producing_time"]=(fields["year"]-fields["init_year"])

#create time to peak variable and time from peak
	#the idea being that affect is different.  

fields["time_to_peak"]=fields["maxyear"]-fields["init_year"]

fields.to_csv("/Users/johannesmauritzen/Google Drive/Research/rOil/data/fields_new.csv")

