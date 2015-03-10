#rr_analysis.py

#attempts using python

#use this?
#http://patsy.readthedocs.org/en/latest/spline-regression.html

#with mixed models in statsmodels

#attempt using rp2
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import random

import rpy2.robjects as robjects

from rpy2.robjects.packages import importr

mgcv = importr("mgcv")


# R code:
# ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
# trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
# group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
# weight <- c(ctl, trt)

# anova(lm.D9 <- lm(weight ~ group))

# summary(lm.D90 <- lm(weight ~ group - 1))# omitting intercept



#rpy2*********
x = robjects.Vector([4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14])
y = robjects.Vector([random.randrange(1,10) for i in range(10)])

fmla = robjects.Formula('y ~ s(x) -1')
env = fmla.environment
env['x'] = x
env['y'] = y

mgcv = importr('mgcv')
fit = mgcv.gam(fmla)

# omitting the intercept
lm_D90 = r.lm("ctl ~ trt")
print(r.summary(lm_D90))

#now with mgcv
fmla = robjects.Formula('y ~ x')
mgcv.gam()




#Using Stan
#http://jofrhwld.github.io/blog/2014/07/29/nonlinear_stan.html

import pystan
import numpy as np
import pandas as pd
intercept = 0.785
deltas = np.array([0, 0.1,0.097, 0.096,0.089]).reshape((5,1))
durations = np.array([50,60,70,80,90]).reshape((5,1))
cumsum=deltas.cumsum().reshape((5,1))
time_mu=cumsum + intercept

durations=np.concatenate((deltas, durations, cumsum, time_mu), axis=1)

test_data = pd.read_table("http://jofrhwld.github.io/data/I_jean.txt")

model_code = """
  data{
    int<lower=0> N; // number of observations
    real y[N];      // the outcome variable
    
    int<lower=0> max_time;  // the largest time index
    int<lower=0> max_word;  // the largest word index
  
    int<lower=0> time[N];    // the time explanatory variable
    int<lower=0> word_id[N]; // the word explanatory variable
  }

  parameters{
    // more or less (1|word) in lmer terms
    vector[max_word] word_effects;

    // scaling parameters for sampling 
    real<lower=0, upper=100> word_sigma;  
    real<lower=0, upper=100> sigma;

    // Ghitza & Gelman used normal(delta[i-1],1) for sampling deltas,
    // but in some other work I found this led to overfitting for my data.
    // So, Im using this hyperprior. 
    real<lower=0, upper=100> delta_sigma;

    // time_deltas is shorter than max_time,
    // because the first delta logically 
    // has to be 0.
    vector[max_time-1] time_deltas;

    real intercept;
  }
  transformed parameters{
    // time_mu will be the expected
    // F1 at each time point
    vector[max_time] time_mu;
    
    // real_deltas is just time_deltas 
    // with 0 concatenated to the front
    vector[max_time] real_deltas;

   
    real_deltas[1] <- 0.0;
    for(i in 1:(max_time-1)){
      real_deltas[i+1] <- time_deltas[i];
    }

    // The cumulative sum of deltas, plus
    // the initial value (intercept) equals
    // the expected F1 at that time index
    time_mu <- cumulative_sum(real_deltas) + intercept;    
  }
  model{
    // this y_hat variable is to allow
    // for vectorized sampling from normal().
    // Sampling is just quicker this way.
    vector[N] y_hat;

    // The first time_delta should be less constrained
    // than the rest. delta_sigma could be very small,
    // and if so, the subsequent delta values would be
    // constrained to be too close to 0.
    time_deltas[1] ~ normal(0, 100);
    for(i in 2:(max_time-1)){
        time_deltas[i] ~ normal(time_deltas[i-1], delta_sigma);
    }
    
    intercept ~ normal(0, 100);
    
    // this is vectorized sampling for all of the
    // word effects.
    word_effects ~ normal(0, word_sigma);
    
    // This loop creates the expected 
    // values of y, from the model
    for(i in 1:N){
      y_hat[i] <- time_mu[time[i]] + word_effects[word_id[i]];
    }

    // this is equivalent to;
    // y[i] <- time_mu[time[i]] + word_effects[word_id[i]] + epsilon[i];
    // epsilon[i] ~ normal(0, sigma);
    y ~ normal(y_hat, sigma);
  }
"""

#transform data

mod_data=test_data
mod_data["dur1"] = (test_data["Dur_msec"]-test_data["Dur_msec"].min())/10+1
mod_data["WordN"] = pd.Series(test_data["Word"], dtype=("category"))

word_dict={"I":0,
           "I'D":1,
           "I'LL":2,
           "I'M":3,
           "I'VE":4}

word_dict[mod_data.Word[0]]
mod_data["WordN"]=[word_dict[i] for i in mod_data.Word]

#data in form of a dict
data_list = {"N" : len(mod_data),
             "y" : mod_data["F1.n"],
             "max_time":int(mod_data["dur1"].max()), 
             "max_word":int(mod_data["WordN"].max()),
             "time": mod_data["dur1"].astype(int),
             "word_id": mod_data["WordN"].astype(int)}

fit = pystan.stan(model_code=model_code, data=data_list,
                  iter=1000, chains=3)


