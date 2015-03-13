#rr_analysis.r

library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv)
library(lubridate) 
library(grid)
library(boot)
library(arm)
library(gamm4)
library(lme4)

#clear workspace
rm(list = ls())

#Use time dummies - first three years, first four years

#Use multilevel (random effects) model.  

#Combine many graphs in a single figure. 

#Showing how a parametric model heavily biases estimation - Put in an online appendix.  

#Instead of splitting up into small and large, use mixed effects to take care of heterogeneity.  

#go straight to preferred set up, splitting up in post, build-out and draw-down.  

#Waiting time model - Time to start modeled as a function of time (bayesian model)

#prep.py

#other models not used

large_fields<-prod_data[prod_data["recoverable_oil"]>3,]
large_fields<-prod_data[prod_data["name"]!="EKOFISK",]



gam_full<-gam(oil_prod_mill_sm3~s(prod_year) + 
     in_place_oil_mill_sm3 + cost_index +
    year + I(year^2) + price + price_l1 + price_l2 + price_l3 + price_l4 + 
     price_l5 + price_l6 + price_l7 + price_l8, 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields, 
     select=TRUE)
summary(gam_full)

gam_full3<-gam(oil_prod_mill_sm3~s(prod_year, in_place_oil_mill_sm3) + 
      cost_index +
     year + I(year^2) + price + price_l1 + price_l2 + price_l3 + price_l4 + 
     price_l5 + price_l6 + price_l7 + price_l8, 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_full3)

