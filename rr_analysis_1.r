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
    year + I(year^2) + I(price/10) + I(price_l1/10) + I(price_l2/10) +
    I(price_l3) + I(price_l4) + I(price_l5/10) + I(price_l6/10) + 
    I(price_l7/10) + I(price_l8/10), 
    family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod)



gam_mod_2d<-gam(oil_prod_mill_sm3~s(prod_year, in_place_oil_mill_sm3) + 
  + I(cost_index/10) +
    year + I(year^2) + (year^3) + I(price/10) + I(price_l1/10) + I(price_l2/10) +
    I(price_l3/10) + I(price_l4/10) + I(price_l5/10) + I(price_l6/10) + 
    I(price_l7/10) + I(price_l8/10), 
    family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_2d)

coef<-gam_mod_2d$coefficients[5:13]
se<-summary(gam_mod_2d)$p.table[5:13,2]
ymin<-coef-2*se
ymax<-coef+2*se
gam_2d_coef<-data.frame(coef=coef, se=se, lag=0:8, ymin=ymin, ymax=ymax)

2d_coef_plot<-ggplot(gam_2d_coef) +
geom_pointrange(aes(x=lag, y=coef, ymin = ymin, ymax = ymax)) +
geom_hline(aes(y=0)) +
theme_bw() +
labs(x="Lags", y="Estimated Coefficient on Oil Price")


#With random effects

gam_mod_re<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
	I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     year + I(year^2) + I(price/10) + I(price_l1/10) + I(price_l2/10) + 
     I(price_l3/10) + I(price_l4/10) + I(price_l5/10) + I(price_l6/10) + 
     I(price_l7/10) + I(price_l8/10), 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_re)

coef<-gam_mod_re$coefficients[6:14]
se<-summary(gam_mod_re)$p.table[6:14,2]
ymin<-coef-2*se
ymax<-coef+2*se
gam_re<-data.frame(coef=coef, se=se, lag=0:8, ymin=ymin, ymax=ymax)

re_coef_plot<-ggplot(gam_re) +
geom_pointrange(aes(x=lag, y=coef, ymin = ymin, ymax = ymax)) +
geom_hline(aes(y=0)) +
theme_bw() +
labs(x="Lags", y="Estimated Coefficient on Oil Price", main="Random Effects Model")

#Now create counterfactuals - 
#oil price production if 30 dollars and 100 dollars


#anova tests with restricted models

gam_mod_re_lim<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
    I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     year + I(year^2) + I(price/10) + I(price_l1/10) + I(price_l2/10) + 
     I(price_l3) + I(price_l4) + I(price_l5/10), 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_re_lim)

gam_mod_re_lim_conc<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
    I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     year + I(year^2) + I(price_l1/10) + I(price_l2/10) + 
     I(price_l3) + I(price_l4) + I(price_l5/10), 
     family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_re_lim_conc)

gam_mod_re_no_price<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
    I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
     year + I(year^2)xz, 
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


#models with interaction effects


gam_mod_2d_inter<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
  + I(cost_index/10) +
    I(price_l1/10) + 
    I(price_l2/10) + I(price_l3/10) + 
    I(price_l4/10) + I(price_l5/10) +
    year + I(year^2) + I(price_l1/10):build_out + 
    I(price_l2/10):build_out + I(price_l3/10):build_out + 
    I(price_l4/10):build_out + I(price_l5/10):build_out, 
    family=gaussian(link=log), weights=in_place_oil_mill_sm3, data=large_fields)
summary(gam_mod_2d_inter)

gam_mod_2d_inter2<-gam(oil_prod_mill_sm3~s(prod_year, in_place_oil_mill_sm3) + 
  + I(cost_index/10) +
    year + I(year^2) + I(price_l1/10) + 
    I(price_l2/10) + I(price_l3/10) + 
    I(price_l4/10) + I(price_l5/10), 
    family=gaussian(link=log), weights=in_place_oil_mill_sm3, 
    data=large_fields[large_fields$build_out==1,])
summary(gam_mod_2d_inter2)

gam_mod_2d_inter3<-gam(oil_prod_mill_sm3~s(prod_year, in_place_oil_mill_sm3) + 
  + I(cost_index/10) +
    year + I(year^2) + I(price_l1/10) + 
    I(price_l2/10) + I(price_l3/10) + 
    I(price_l4/10) + I(price_l5/10), 
    family=gaussian(link=log), weights=in_place_oil_mill_sm3, 
    data=large_fields[large_fields$build_out==0,])
summary(gam_mod_2d_inter3)

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



