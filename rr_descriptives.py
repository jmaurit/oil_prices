#rr_descriptives.py

#Descriptives for revise and resubmit

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
import sys
import seaborn as sns
import matplotlib.pyplot as plt
import random
import statsmodels.api as sm



#first on individual field data
field_data=pd.read_csv('/Users/johannesmauritzen/research/oil_prices/data/field_data.csv')
prod_data=pd.read_csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

#remove fields with zero oil

field_data=field_data[field_data["in_place_oil_mill_sm3"]!=0]

#histogram of size, in terms of in place oil
plt.hist(field_data["in_place_oil_mill_sm3"], 50, facecolor='green', alpha=0.5)
plt.hist(field_data["recoverable_oil"], 50, facecolor='red', alpha=0.5)
plt.show()

#show scatter between recoverable and in_place_oil
fig, ax = plt.subplots()
ax.scatter(x=field_data["in_place_oil_mill_sm3"], y=field_data["recoverable_oil"])
plt.show()

# Fit and summarize OLS model
X=field_data["in_place_oil_mill_sm3"]
X=sm.add_constant(X)
Y=field_data["recoverable_oil"]
mod = sm.OLS(Y,X)
results = mod.fit()
fitted=np.column_stack((results.fittedvalues, mod.exog[:,1]))
x_fit=np.linspace(0, 1200, 100)
a,b=results.params
y_fit = a+x_fit*b




fig, ax = plt.subplots()
ax.scatter(x=field_data["in_place_oil_mill_sm3"], y=field_data["recoverable_oil"])
ax.plot(x_fit, y_fit)
ax.annotate('dy/dx~0.52', xy=(400,300), xytext=(300,300))
fig.set_size_inches(5,4)
fig.savefig('/Users/johannesmauritzen/research/oil_prices/figures/in_place_vs_recov.png',dpi=100)

plt.show()

print res.summary()


sns.lmplot("in_place_oil_mill_sm3", "recoverable_oil", field_data, 
	scatter_kws={"marker": ".", "color": "slategray"},
          line_kws={"linewidth": 1, "color": "seagreen"})
sns.axlabel(xlabel='In-place Oil (mill SM3)', ylabel='Estimated Recoverable Oil (mill SM3)')
plt.show()

plt.close()


