library(ggplot2) #for plotting
library(mgcv) #for gam modeling
library(lubridate) #for extracting "year" component
library(dplyr) 
library(reshape2)
library(zoo)
library(sandwich)
library(grid)

#make function that inputs simulated field_size and outputs simulated
#first production year
#should be so that large fields are found more often early on.  

gen_year<-function(size, maxsize){
	#Generates initial year as a function of size/maxsize
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
	#Given size of field, generates production shape based on
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
add_uncertainty<-function(beta, fields){ 
	fields$prod<-fields$prod_shape*
		exp(beta*(fields$prices))*
			rlnorm(length(fields$prices), meanlog=0, sdlog=.10)
	return(fields)
}

run_gam<-function(formula, fields){
	#runs GAM model and returns coefficient on price
	# formula<-formula(prod~s(prod_years, bs="cr") + prices + size)

	# beta<-0.05
	# fields<-add_uncertainty(fields=sim_fields, beta=0)

	gam_sim<-gam(formula,
		family=gaussian(link=log), weights=size, data=fields, 
		na.action='na.omit')
	coefficients(gam_sim)
	return(coefficients(gam_sim)["prices"])
}

run_glm<-function(formula, fields){
	#test
	#formula <- formula_glm
	#fields <- sim_fields_unc
	#test
	
	glm_sim<-lm(formula, data=fields)
	return(coefficients(glm_sim)["prices"])
}

mc_run<-function(formula, use_gam=TRUE, fields, beta){
	#adds uncertainty then runs regression
	#test

	# use_gam=TRUE
	#
	fields_unc<-add_uncertainty(fields=fields, beta=beta)
	if(use_gam){
		return(run_gam(formula, fields_unc))
	}
	else{
		return(run_glm(formula, fields_unc))
	}
}

#****************************************************

oil_price<-read.csv("research/oil_prices/data/oil_price.csv")


max<-1267
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
sim_fields_unc<-add_uncertainty(fields=sim_fields, beta=0)

simulated_production<-ggplot(sim_fields_unc)+
 geom_line(aes(x=year, y=prod, color=factor(name)),alpha=.3, size=1) +
 guides(color=FALSE) +
 scale_color_grey(start=0, end=.3) +
 theme_bw() +
 labs(x="", y="Simulated Production from Fields")

formula_1<- formula(prod~s(prod_years, name, bs="re") +prices + size + s(year, bs="cr", k=4))
formula_2<- formula(prod~s(prod_years, bs="cr") + prices + size + s(year, bs="cr", k=4))
formula_3<- formula(prod~s(prod_years, bs="cr") + prices + size)

sim_fields_unc<-add_uncertainty(fields=sim_fields, beta=0)
run_gam(formula=formula_3, fields=sim_fields_unc)

gamm_mc_beta0<-replicate(100, mc_run(beta=0, formula=formula_1, fields=sim_fields, use_gam=TRUE))
gamm_mc_beta05<-replicate(100, mc_run(beta=.05, formula=formula_1, fields=sim_fields, use_gam=TRUE))
gamm_mc_beta0<-as.data.frame(gamm_mc_1)
gamm_mc_data<-data.frame(beta0=gamm_mc_beta0, beta05=gamm_mc_beta05)
#write.csv(gamm_mc_data, "/Users/johannesmauritzen/research/oil_prices/mc_data.csv")
gam_mc_data<-read.csv("/Users/johannesmauritzen/research/oil_prices/mc_data.csv")
gam_mc_data$X<-NULL
gam_mc_data<-melt(gam_mc_data)
colnames(gam_mc_data)<-c("beta", "coefficient")
levels(gam_mc_data$beta)<-c("0", "0.05")
levels(gam_mc_data)

gam_mc_plot<-ggplot(gam_mc_data, aes(x=coefficient, fill=beta)) +
geom_density(alpha=.5, size=0) +
geom_vline(aes(xintercept=c(0, 0.05))) +
xlab("beta_hat, estimated by GAM") +
ylab("") +
scale_fill_grey() +
theme_bw()

formula_glm <- formula(prod ~ prod_years + I(prod_years^2) + I(prod_years^3) + 
	I(prod_years^4) + size + prices + year + I(year^2) + I(year^3))

glm_mc_beta0<-replicate(100, mc_run(beta=0, formula=formula_glm, fields=sim_fields, use_gam=FALSE))
glm_mc_beta05<-replicate(100, mc_run(beta=0.05, formula=formula_glm, fields=sim_fields, use_gam=FALSE))

glm_mc_data<-data.frame(beta0=glm_mc_beta0, beta05=glm_mc_beta05)
glm_mc_data<-melt(glm_mc_data)
colnames(glm_mc_data)<-c("beta", "coefficient")
levels(glm_mc_data$beta)<-c("0", "0.05")


glm_mc_plot<-ggplot(glm_mc_data, aes(x=coefficient, fill=beta)) +
geom_density(alpha=.5, size=0) +
geom_vline(aes(xintercept=c(0, 0.05))) +
scale_fill_grey() +
theme_bw() +
guides(fill=FALSE) +
xlab("beta_hat, estimated by GLM") +
ylab("Density")

png("/Users/johannesmauritzen/research/oil_prices/figures/mc_plot.png", 
	width = 30, height = 30, units = "cm", res=150, pointsize=12)
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(2, 2)))
	vplayout <- function(x, y)
	 viewport(layout.pos.row = x, layout.pos.col = y)
	print(simulated_production, vp = vplayout(1,1:2))
	print(glm_mc_plot, vp = vplayout(2, 1))
	print(gam_mc_plot, vp = vplayout(2, 2))
dev.off()










