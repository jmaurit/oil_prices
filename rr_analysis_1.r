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
library(texreg)

field_data<-read.csv('/Users/johannesmauritzen/research/oil_prices/data/field_data.csv')

#How much of the total oil reserves are we excluding
tot_rec_oil<-sum(field_data["recoverable_oil"])
small_rec_oil<-sum(field_data["recoverable_oil"][field_data["recoverable_oil"]<=3])
28.3/4779.2
#.5% of total recoverable oil.  

prod_data<-read.csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

large_fields<-prod_data[prod_data["recoverable_oil"]>3,]
large_fields<-prod_data[prod_data["name"]!="EKOFISK",]


gam_mod<-gam(oil_prod_mill_sm3~s(prod_year, bs="cr") + 
	I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
    s(year, bs="cr", k=4) + I(price/10) + I(price_l1/10) + I(price_l2/10) +
    I(price_l3) + I(price_l4) + I(price_l5/10) + I(price_l6/10) + 
    I(price_l7/10) + I(price_l8/10), 
    family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod)

#With random effects

gam_mod_re<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
	I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     s(year, bs="cr", k=4) + I(price/10) + I(price_l1/10) + I(price_l2/10) + 
     I(price_l3) + I(price_l4) + I(price_l5/10) + I(price_l6/10) + 
     I(price_l7/10) + I(price_l8/10), 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_re)

#anova tests with restricted models

gam_mod_re_lim<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
    I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     s(year, bs="cr", k=4) + I(price/10) + I(price_l1/10) + I(price_l2/10) + 
     I(price_l3) + I(price_l4) + I(price_l5/10), 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_re_lim)

gam_mod_re_lim_conc<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
    I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     s(year, bs="cr", k=4) + I(price_l1/10) + I(price_l2/10) + 
     I(price_l3) + I(price_l4) + I(price_l5/10), 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_re_lim_conc)

gam_mod_re_no_price<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
    I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     s(year, bs="cr", k=4), 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_re_no_price)

#anova modeling
anova_mod1<-anova(gam_mod_re, gam_mod_re_lim, test="F")
anova_mod2<-anova(gam_mod_re, gam_mod_re_lim_conc, test="F")
anova_mod3<-anova(gam_mod_re_lim_conc,gam_mod_re_no_price, test="F")

#list of results
results<-list(gam_mod, gam_mod_re, gam_mod_re_lim_conc)
texreg(results, 
    custom.model.names = c("w/out Rand. Effects", "w Rand. Effects", "Lags 1-5"),
    digits = 3,
    label = "GAM_model_table")

#show model fit and other indicators of preferred model


#can directly interpret simple model gam_mod
plot(gam_mod)

#plot coefficients
gam_coef<-gam_mod_re$coefficients[4:12]
gam_se<-summary(gam_mod_re)$se[4:12]
lag<-c("t", "t-1", "t-2", "t-3", "t-4", "t-5", "t-6", "t-7", "t-8")
min<-gam_coef-2*gam_se
max<-gam_coef+2*gam_se

price_est<-data.frame(gam_coef=gam_coef, gam_se=gam_se, min=min, max=max, lag=lag)

ggplot(price_est) + 
    geom_pointrange(aes(x=lag,y=gam_coef,ymin=min, ymax=max), size=2) +
    geom_abline(intercept=0) +
    ylab("Oil Price Coefficient Estimate")

pref_mod<-gam_mod_re_lim_conc
plot(pref_mod)
summary(pref_mod)


#show scenarios fits

#fitted vs. predicted
act_vs_fitted<-ggplot() +
    geom_point(aes(x=fitted(pref_mod), y=pref_mod$y)) +
    geom_abline(intercept=0, slope=1) +
    labs(x = "Fitted Values", y = "Actual") 


png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/over_fitted.png",
width = 35, height = 21, units = "cm", res=150, pointsize=10) 
     #  fitted vs. reside
dev.off()

#sim
chart_under_6<-sim_gam(model=gam_price_under_2d,start=2,stop=8)




