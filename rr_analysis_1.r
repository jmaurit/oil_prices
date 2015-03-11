#rr_analysis_1.r

#last update march 11th, 2015
#First new modeling
rm(list = ls())

library(ggplot2)
library(dplyr)
library(reshape2)
library(mgcv)
library(lubridate) 
library(grid)
library(boot)
library(arm)
library(gamm4)
library(lme4)

prod_data<-read.csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

head(prod_data)

#all fields

gam_full<-gam(oil_prod_mill_sm3~s(producing_time, bs="cr") + s(recoverable_oil) + 
     s(year, bs="cr") + price + price_l1 + price_l2 + price_l3 + price_l4 + 
     price_l5 + price_l6 + price_l7 + price_l8, 
     family=gaussian(link=log), weights=recoverable_oil, data=prod_data)
summary(gam_full)





