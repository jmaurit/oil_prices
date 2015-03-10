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

fields<-read.csv("/Users/johannesmauritzen/Google Drive/Research/rOil/data/fields_new.csv")



large_fields<-fields[fields$recoverable_oil>5,]
large_fields<-large_fields[large_fields$name!="EKOFISK",]
large_fields<-large_fields[!is.na(large_fields$year_prod),]

#simple GAM
gam_full<-gam(year_prod~s(producing_time, bs="cr") + s(recoverable_oil) + 
     s(year, bs="cr") +
oil_price_real + oil_price_real_l1 + oil_price_real_l2 + 
oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 + oil_price_real_l6 +
oil_price_real_l7 + oil_price_real_l8, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_full)

gam_2<-gam(year_prod ~ s(producing_time, bs="cr") + recoverable_oil + 
     s(year, bs="cr") +oil_price_real_l2, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_2)

gam_2_alt<-gam(year_prod ~ s(producing_time, bs="cr") + recoverable_oil + 
     year + I(year^2) +oil_price_real_l2, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_2_alt)

gam_full_alt<-gam(year_prod ~ s(producing_time, bs="cr") + s(recoverable_oil) + 
     I(year) + I(year^2) + I(year^3)+
      oil_price_real + oil_price_real_l1 + oil_price_real_l2 + 
oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 + oil_price_real_l6 +
oil_price_real_l7 + oil_price_real_l8, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_full_alt)
plot(gam_full_alt)

gam_1_4_alt<-gam(year_prod ~ s(producing_time, bs="cr") + recoverable_oil + 
     I(year) + I(year^2) + I(year^3)
     oil_price_real_l1 + oil_price_real_l2 + 
oil_price_real_l3 + oil_price_real_l4, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_1_4_alt)

gam_0_4_alt<-gam(year_prod ~ s(producing_time, bs="cr") + recoverable_oil + 
     I(year) + I(year^2) + I(year^3) + oil_price_real +
     oil_price_real_l1 + oil_price_real_l2 + 
oil_price_real_l3 + oil_price_real_l4, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_0_4_alt)

anova(gam_1_4_alt, gam_full_alt, test="F")
anova(gam_1_4_alt, gam_0_4_alt, test="F")


gam_2_data<-gam_2$model

gam_1_3<-gam(year_prod ~ s(producing_time, bs="cr") + recoverable_oil + 
     s(year, bs="cr") +
     oil_price_real_l2 + oil_price_real_l1 + oil_price_real_l3, 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)

gam_1_3_data<-gam_1_3$model

gam_0<-gam(year_prod~s(producing_time, bs="cr") +
     + recoverable_oil + s(year, bs="cr"), 
     family=gaussian(link=log), weights=recoverable_oil, data=gam_2_4_data)
#summary(gam_0)
anova(gam_0, gam_2, test="F")
anova(gam_2, gam_1_3, test="F")

anova(gam_2_4, gam_full, test="F")
anova(gam_0, gam_2_4, test="F")

anova(gam_2, gam_full, test="F")

#with random effect
gam_re<-gam(year_prod ~ s(producing_time, name, bs="re") +
     recoverable_oil + I(year) + I(year^2) + I(year^3) + 
  oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4, 
     family=gaussian(link=log), data=large_fields)
summary(gam_re)
plot(gam_re)


gam.check(gam_re)

gam_re_null<-gam(year_prod ~ s(producing_time, name, bs="re") +
     recoverable_oil + s(year, bs="cr"), 
     family=gaussian(link=log), weights=recoverable_oil, data=large_fields)
summary(gam_re)



#Using lme4
prod_lme<-lmer(year_prod ~ producing_time +  
     I(producing_time^2) + I(producing_time^3) + 
     I(producing_time^4) + I(producing_time^5) +
     I(producing_time^6) +
     oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
      (1+producing_time|name),
      data=large_fields)
summary(prod_lme)


#simple GAMM using GAMM4
prod_gamm<-gamm4(year_prod ~ s(producing_time),
	family=gaussian(link=log), random=~(1|year),
	 data=large_fields)




#using GAMM
prod_gamm<-gamm(year_prod~ producing_time + 
     I(producing_time^2) + I(producing_time^3) + s(year),
	family=gaussian(link=log), random=list(name=~1),niterPQL=50,
	 data=large_fields)

large_fields =fields[]

#STAN non-linear functions
#Ghitza and Gelman using STAN and non-linear
#http://jofrhwld.github.io/blog/2014/07/29/nonlinear_stan.html


     library(gamm4)
     
     set.seed(0) 
     dat <- gamSim(1,n=400,scale=2) ## simulate 4 term additive truth
     ## Now add 20 level random effect `fac'...
     dat$fac <- fac <- as.factor(sample(1:20,400,replace=TRUE))
     dat$y <- dat$y + model.matrix(~fac-1)%*%rnorm(20)*.5
     
     br <- gamm4(y~s(x0)+x1+s(x2),data=dat,random=~(1|fac))
     plot(br$gam,pages=1)

     summary(br$gam) ## summary of gam
     summary(br$mer) ## underlying mixed model
     anova(br$gam) 
     
     ## compare gam fit of the same
     bg <- gam(y~s(x0)+x1+s(x2)+s(fac,bs="re"),
               data=dat,method="REML")
     plot(bg,pages=1)
     gam.vcomp(bg)
     
     ##########################
     ## Poisson example GAMM...
     ##########################
     ## simulate data...
     x <- runif(100)
     fac <- sample(1:20,100,replace=TRUE)
     eta <- x^2*3 + fac/20; fac <- as.factor(fac)
     y <- rpois(100,exp(eta))
     
     ## fit model and examine it...
     bp <- gamm4(y~s(x),family=poisson,random=~(1|fac))
     plot(bp$gam)
     bp$mer

#using nlmer - cumulative production function as a logisitic growth function
#http://lme4.r-forge.r-project.org/slides/2009-07-01-Lausanne/8NLMMD.pdf


