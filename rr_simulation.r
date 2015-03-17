library(ggplot2) #for plotting
library(mgcv) #for gam modeling
library(lubridate) #for extracting "year" component
library(dplyr) 
library(reshape2)
library(zoo)
library(sandwich)

#make function that inputs simulated field_size and outputs simulated
#first production year
#should be so that large fields are found more often early on.  

gen_year<-function(size, maxsize){
	#test
	#size=15
	#maxsize=1267
	#

	#let small fields be distributed uniformly from 1975 to 2013
	if(size<10){
		year<-trunc(runif(1, 1975, 2008))	
	} else{	#while large fields should be more common earlier on
		range<-FALSE
		while(range==FALSE){
			year<-trunc(rnorm(1,mean=(1973+(maxsize+300)/(size+300)), sd=10))
			ifelse(year>=1970 & year<=2013, range<-TRUE, range<-FALSE)
			#print(paste("in while loop", year))
			}	
		}
return(year)	
}


#Simulation of data****************************************************************

gen_production_shape<-function(field){
	#
	#field<-c(20, 1990, 1)
	#
	size<-as.numeric(field[1])
	start<-as.numeric(field[2])
	name<-field[3]

	t<-trunc(sqrt(size)) + 3
	time<- -t:(t)
	year<-start:(start+2*t-1)

	#use a cumulative logistic function to represent shape of function
	cum_production <- 1/(1+exp(-(time)/3))*size

	#take difference to create production per year
	prod_shape <- diff(cum_production)
	prod_years <- 1:length(prod_shape)
	production<-data.frame(year, prod_shape, name, size, prod_years)
	return(production)
}



#GAM simualtion *********************************
add_uncertainty<-function(beta0=0, fields){ 
	fields$prod<-fields$prod_shape*
		(exp(beta0*(fields$price))*
			rlnorm(length(fields$price), meanlog=0, sdlog=.05))
	return(fields)
}

run_gam<-function(formula, fields, beta=0){
	#
	#fields <- sim_fields_unc
	#formula<-formula(prod ~ s(prod_time) + size + prices)
	#
	gam_sim<-gam(formula,
		family=gaussian(link=log), weights=size, data=fields, 
		na.action='na.omit')
	return(coefficients(gam_sim)["prices"])
}



run_glm<-function(formula, fields, beta=0){
	return(NA)
}

#****************************************************


field_size<-round(exp(rnorm(40,mean=4, sd=1.5)), digits=1)
init_year<-trunc(sapply(field_size, gen_year, maxsize=max))
fields<-cbind(field_size, init_year,1:length(init_year))

#real price series
prices<-oil_price[c("year", "oil_price_real")]
names(prices)<-c("year", "prices")

#make units into 10 dollars units
prices$prices<-prices$prices/10

sim_fields<-apply(fields, 1, gen_production_shape)
sim_fields<-Reduce(rbind, sim_fields)
sim_fields<-merge(sim_fields, prices, by="year")
sim_fields<-sim_fields[order(sim_fields$name, sim_fields$year),]

#test adding uncertainty and chart
sim_fields_unc<-add_uncertainty(sim_fields, beta=0)

simulated_production<-ggplot(sim_fields_unc)+
 geom_line(aes(x=year, y=prod, color=factor(name)),alpha=.3) +
 guides(color=FALSE) +
 labs(x="", y="Simulated Production from Fields")

png("/Users/johannesmauritzen/research/oil_prices/figures/simulated_production.png", 
	width = 27.81, height = 21, units = "cm", res=150, pointsize=10)
simulated_production
dev.off()

formula_1= formula(prod~s(prod_years) + size + prices)

gamm_mc_1<-replicate(5, run_gam(beta=0, formula=formula_1, fields=sim_fields_unc))

gamm_mc_1<-as.data.frame(gamm_mc_1)

gam_model_price_mc<-ggplot(gamm_mc_1, aes(x=gamm_mc_1)) + 
geom_histogram(aes(y=..density..)) +
geom_density() +
xlab("Estimated Coefficient on Price")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_model_price_mc.png", 
	width = 27.81, height = 21, units = "cm", res=50, pointsize=10)
gam_model_price_mc
dev.off()


formula_0=formula(prod~time_to_peak + time_to_peak_sq + time_to_peak_cu + 
	peak_to_end + peak_to_end_sq + peak_to_end_cu +size + price)





gamm_mc_0<-replicate(1000, gam_mc(beta=0, formula=formula_0, 
	fields=sim_fields, prices=prices))

gamm_mc_0<-as.data.frame(gamm_mc_0)

lin_model_price_mc<-ggplot(gamm_mc_0, aes(x=gamm_mc_0)) + 
geom_histogram(aes(y=..density..)) +
geom_density() +
xlab("Estimated Coefficient on Price")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/lin_model_price_mc.png", 
	width = 27.81, height = 21, units = "cm", res=50, pointsize=10)
lin_model_price_mc
dev.off()









