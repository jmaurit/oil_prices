#rr_analysis_1.r

#last update march 12th, 2015
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
library(nlme)

prod_data<-read.csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

head(prod_data)

#all fields

large_fields<-prod_data[prod_data["recoverable_oil"]>3,]
large_fields<-prod_data[prod_data["name"]!="EKOFISK",]



gam_full<-gam(oil_prod_mill_sm3~s(prod_year) + 
	in_place_oil_mill_sm3 + cost_index +
    year + I(year^2) + price + price_l1 + price_l2 + price_l3 + price_l4 + 
     price_l5 + price_l6 + price_l7 + price_l8, 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields, 
     select=TRUE)
summary(gam_full)

gam_full2<-gam(oil_prod_mill_sm3~s(prod_year, bs="cr") + 
	in_place_oil_mill_sm3 + cost_index +
    s(year, bs="cr", k=4) + price + price_l1 + price_l2 + price_l3 + price_l4 + 
     price_l5 + price_l6 + price_l7 + price_l8, 
     family=gaussian(link=log), weights=recoverable_oil, 
     data=large_fields)
summary(gam_full2)

gam_full3<-gam(oil_prod_mill_sm3~s(prod_year, in_place_oil_mill_sm3) + 
	 cost_index +
     year + I(year^2) + price + price_l1 + price_l2 + price_l3 + price_l4 + 
     price_l5 + price_l6 + price_l7 + price_l8, 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_full3)

#With random effects

gam_full_re<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
	in_place_oil_mill_sm3 + cost_index +
    year + I(year^2) + price + price_l1 + price_l2 + price_l3 + price_l4 + 
     price_l5 + price_l6 + price_l7 + price_l8, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_full_re)

#approach with nlme
#http://lme4.r-forge.r-project.org/slides/2009-07-01-Lausanne/8NLMMD.pdf
#logistic growth model

c(Asym = 200, xmid = 725, scal = 350)
nm1 <- nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ recoverable_oi|Tree,
                  Orange, start = startvec))

startvec <- c(price= 0.003, price_l1=0.003, price_l2=0.003, 
	price_l3=0.003, price_l4=0.003, in_place_oil_mill_sm3=0.003, year=28)

ln_mod1<-nlmer(oil_prod_mill_sm3 ~ SSlogis( 
	year + price + price_l1 + price_l2 + price_l3 + price_l4) ~
	price + price_l1 + price_l2 + price_l3 + price_l4| name, large_fields, 
	start=startvec)

startvec <- c(Asym = 200, xmid = 725, scal = 350)
     (nm1 <- nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
                  Orange, start = startvec))


