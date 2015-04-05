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

large_fields<-prod_data[prod_data["recoverable_oil"]>5,]
large_fields<-prod_data[prod_data["name"]!="EKOFISK",]


gam_mod<-gam(oil_prod_mill_sm3~s(prod_year, bs="cr") +
	I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
    s(year, bs="cr", k=4) + I(price/10) + I(price_l1/10) + I(price_l2/10) +
    I(price_l3/10) + I(price_l4/10) + I(price_l5/10) + I(price_l6/10) + 
    I(price_l7/10) + I(price_l8/10), 
    family=gaussian(link=log), weights=I(in_place_oil_mill_sm3/100), data=large_fields)
summary(gam_mod)
#,weights=I(in_place_oil_mill_sm3/100)

#terms

pdat1 <- with(large_fields,
             data.frame(prod_year = round(seq(min(prod_year), max(prod_year), length = 200)),
                        year = rep(mean(year), 200),
                        oil_prod_mill_sm3 = rep(mean(oil_prod_mill_sm3), 200),
                        in_place_oil_mill_sm3 = rep(mean(in_place_oil_mill_sm3), 200),
                        cost_index = rep(mean(cost_index), 200),
                        price = rep(mean(price), 200),
                        price_l1 = rep(mean(price_l1), 200),
                        price_l2= rep(mean(price_l2), 200),
                        price_l3= rep(mean(price_l3), 200),
                        price_l4 = rep(mean(price_l4), 200),
                        price_l5= rep(mean(price_l5), 200),
                        price_l6 = rep(mean(price_l6), 200),
                        price_l7 = rep(mean(price_l7), 200),
                        price_l8= rep(mean(price_l8), 200)
                        ))

pred1 <- predict(gam_mod, pdat1, type = "terms", se.fit = TRUE)
s_prod_year<-pred1$fit[,12]
prod_year<-pdat1$prod_year
se_s<-pred1$se[,12]
smooth_pred1<-data.frame(s_prod_year = s_prod_year, prod_year=prod_year, 
    conf_plus=(s_prod_year +2*se_s),
    conf_minus=(s_prod_year -2*se_s))


prod_smooth<-ggplot(smooth_pred1, aes(x=prod_year))+
    geom_ribbon(aes(ymin=conf_minus, ymax=conf_plus), alpha=.5, fill="grey") +
    geom_line(aes(y=s_prod_year)) + 
    labs(x="Production Years", y="Smooth Function of Production Time") +
    theme_bw() 

pdat2 <- with(large_fields,
             data.frame(prod_year = rep(mean(prod_year), 200),
                        year = round(seq(min(year), max(year), length = 200)),
                        oil_prod_mill_sm3 = rep(mean(oil_prod_mill_sm3), 200),
                        in_place_oil_mill_sm3 = rep(mean(in_place_oil_mill_sm3), 200),
                        cost_index = rep(mean(cost_index), 200),
                        price = rep(mean(price), 200),
                        price_l1 = rep(mean(price_l1), 200),
                        price_l2= rep(mean(price_l2), 200),
                        price_l3= rep(mean(price_l3), 200),
                        price_l4 = rep(mean(price_l4), 200),
                        price_l5= rep(mean(price_l5), 200),
                        price_l6 = rep(mean(price_l6), 200),
                        price_l7 = rep(mean(price_l7), 200),
                        price_l8= rep(mean(price_l8), 200)
                        ))

pred2 <- predict(gam_mod, pdat2, type = "terms", se.fit = TRUE)
s_year<-pred2$fit[,13]
year<-pdat2$year
se_s<-pred2$se[,13]
smooth_pred2<-data.frame(s_year = s_year, year=year, 
    conf_plus=(s_year +2*se_s),
    conf_minus=(s_year -2*se_s))

cal_smooth<-ggplot(smooth_pred2, aes(x=year))+
    geom_ribbon(aes(ymin=conf_minus, ymax=conf_plus), alpha=.5, fill="grey") +
    geom_line(aes(y=s_year)) + 
    labs(x="Years", y="Smooth Function of Calendar Time") +
    ylim(-3,1) +
    theme_bw() 

png("/Users/johannesmauritzen/research/oil_prices/figures/smooths.png", 
    width = 25, height = 20, units = "cm", res=100, pointsize=12)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 1)))
    vplayout <- function(x, y)
      viewport(layout.pos.row = x, layout.pos.col = y)
    print(prod_smooth, vp = vplayout(1, 1))
    print(cal_smooth, vp = vplayout(2, 1))
dev.off()


gam_mod_2d<-gam(oil_prod_mill_sm3~s(prod_year, in_place_oil_mill_sm3) +
    s(year, bs="cr", k=4) +
    I(cost_index/10) +
     I(price/10) + I(price_l1/10) + I(price_l2/10) +
    I(price_l3/10) + I(price_l4/10) + I(price_l5/10) + I(price_l6/10) + 
    I(price_l7/10) + I(price_l8/10), 
    family=gaussian(link=log), weights=I(in_place_oil_mill_sm3/100), data=large_fields)
summary(gam_mod_2d)
#, weights=I(in_place_oil_mill_sm3/100)
#year + I(year^2) +

coef<-gam_mod_2d$coefficients[5:13]
se<-summary(gam_mod_2d)$p.table[5:13,2]
ymin<-coef-2*se
ymax<-coef+2*se
gam_2d_coef<-data.frame(coef=coef, se=se, lag=0:8, ymin=ymin, ymax=ymax, type="2d Smooth Model")

coef_plot_2d<-ggplot(gam_2d_coef) +
geom_pointrange(aes(x=lag, y=coef, ymin = ymin, ymax = ymax)) +
geom_hline(aes(y=0)) +
theme_bw() +
labs(x="Lags", y="Estimated Coefficient on Oil Price", 
    title="2-dim. Smooth Model")


#With random effects

gam_mod_re<-gam(oil_prod_mill_sm3~s(prod_year, name, bs="re") + 
    s(year, bs="cr", k=4) +
	I(in_place_oil_mill_sm3/100) + I(in_place_oil_mill_sm3/100) + I(cost_index/10) +
    I(price/10) + I(price_l1/10) + I(price_l2/10) +
    I(price_l3/10) + I(price_l4/10) + I(price_l5/10) + I(price_l6/10) + 
    I(price_l7/10) + I(price_l8/10), 
     family=gaussian(link=log), weights=I(in_place_oil_mill_sm3/100), data=large_fields)
summary(gam_mod_re)

#weights=I(in_place_oil_mill_sm3/100) ,

coef<-gam_mod_re$coefficients[4:12]
se<-summary(gam_mod_re)$p.table[4:12,2]
ymin<-coef-2*se
ymax<-coef+2*se
gam_re<-data.frame(coef=coef, se=se, lag=0:8, ymin=ymin, ymax=ymax, type="Random Effects Model")

model_coef<-rbind(gam_re, gam_2d_coef)

re_coef_plot<-ggplot(gam_re) +
geom_pointrange(aes(x=lag, y=coef, ymin = ymin, ymax = ymax)) +
geom_hline(aes(y=0)) +
theme_bw() +
labs(x="Lags", y="Estimated Coefficient on Oil Price", 
   title="Random Effects Model")

coef_plot<-ggplot(model_coef) +
geom_pointrange(aes(x=lag, y=coef, ymin = ymin, ymax = ymax)) +
geom_hline(aes(y=0)) +
facet_wrap(~type, nrow=2) +
theme_bw() +
labs(x="Lags", y="Estimated Coefficient on Oil Price")

png("/Users/johannesmauritzen/research/oil_prices/figures/price_coefficents.png",
width = 25, height =15, units = "cm", res=150, pointsize=12) 
print(coef_plot)
dev.off()


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
results<-list(gam_mod, gam_mod_re, gam_mod_2d)
texreg(results, 
    custom.model.names = c("w/out Rand. Effects", "w Rand. Effects", "2-d Smooth"),
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



