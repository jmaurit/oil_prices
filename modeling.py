#modeling.py

#first run prep.py
ipython



%run prep.py #opens data and manipulates, loads standard python libraries

from rpy2.robjects import FloatVector
from rpy2.robjects import Vector
from rpy2.robjects import StrVector

from rpy2.robjects.packages import importr
stats = importr('stats')
base = importr('base')

ctl = FloatVector([4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14])
trt = FloatVector([4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69])
group = base.gl(2, 10, 20, labels = ["Ctl","Trt"])
weight = ctl + trt

robjects.globalenv["weight"] = weight
robjects.globalenv["group"] = group
lm_D9 = stats.lm("weight ~ group")
print(stats.anova(lm_D9))

# omitting the intercept
lm_D90 = stats.lm("weight ~ group - 1")
print(base.summary(lm_D90))

coef=lm_D9.rx2('coefficients')

#Now try with mgcv
mgcv = importr('mgcv')

#variables
robjects.globalenv["year_prod"]  = FloatVector(np.array(fields["year_prod"]))
robjects.globalenv["producing_time"]  = FloatVector(np.array(fields["producing_time"]))
robjects.globalenv["recoverable_oil"] = FloatVector(np.array(fields["recoverable_oil"]))

#prod_gam_under<-gam(year_prod~s(producing_time, recoverable_oil) + s(peak_to_end, recoverable_oil) +  s(year), 
#	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

stat_mod=stats.lm("year_prod ~ producing_time + recoverable_oil")
gam_mod=mgcv.gam("year_prod ~ s(producing_time)", family=gaussian(link=log))





