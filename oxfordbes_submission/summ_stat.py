#summ_stat.py

import pandas as pd
import tabulate as tab
import numpy as np
import matplotlib as mpt
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns
import statsmodels.formula.api as smf
import statsmodels.api as sm
import sys
import pystan
import scipy
import pickle
import json

pd.options.display.max_rows = 2000
pd.options.display.max_columns = 100

#statsmodels model
field_data = pd.read_csv('/Users/johannesmauritzen/research/oil_prices/data/field_data.csv')
prod_data = pd.read_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

large_prod=prod_data[prod_data["recoverable_oil"]>5]
tot_prod = large_prod.groupby("name")["oil_prod_mill_sm3"].sum()

large_field = field_data[field_data.recoverable_oil>5]
large_field = large_field[large_field.producing_date.notnull()]
large_field["producing_date"] = pd.to_datetime(large_field.producing_date, format="%d.%m.%Y")
large_field["prod_time"] = pd.to_datetime("2015-01-01") - large_field.producing_date
large_field.reset_index(inplace=True)

prod_years = [float(d.days)/365 for d in large_field.prod_time]
large_field["prod_years"] = prod_years

summ_data = np.empty(shape=(4,5))

def get_percentiles(data):
	return(np.percentile(data, [5, 25, 50, 75, 95]).round(2))

summ_data[0 ,:] = get_percentiles(large_field.in_place_oil_mill_sm3)
summ_data[1 ,:] = get_percentiles(large_field.recoverable_oil)
summ_data[2 ,:] = get_percentiles(large_field.prod_years)
summ_data[3, :] = get_percentiles(tot_prod)

table = pd.DataFrame(summ_data, columns =["5%", "25%", "50%", "75%", "95%"])
table["variable"] = ["In Place Oil, Mill SM3", "Est. Recoverable Oil", "Production Years, to 2015", "Tot. Cumulative Prod., to 2015"]
table.set_index("variable", inplace=True)
table.to_latex(buf="oxfordbes_submission/summ_stat.tex")
 #rps_laws[['Total RPS %', 'Year','voluntary']].to_latex(buf="temp_tables.tex")


