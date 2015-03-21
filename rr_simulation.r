library(ggplot2) #for plotting
library(mgcv) #for gam modeling
library(lubridate) #for extracting "year" component
library(dplyr) 
library(reshape2)
library(zoo)
library(sandwich)
library(grid)
library(rmarkdown)

#render rmd file
rmarkdown::render("/Users/johannesmauritzen/research/oil_prices/mc_sim.rmd")
#make function that inputs simulated field_size and outputs simulated

#first production year
#should be so that large fields are found more often early on.  

gen_year<-function(size){
	#while large fields should be more common earlier - from a normal dist.
	#with mean at 1970
	#constrained to be between 1970 and 2013
	range<-FALSE
	while(range==FALSE){
		year<-trunc(rnorm(1,mean=(1970), sd=10))
		ifelse(year>=1970 & year<=1990, range<-TRUE, range<-FALSE)
		}
		
return(year)
}


#Simulation of data****************************************************************

gen_production_shape<-function(field){
	#Given size of field, generates production shape
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
			rlnorm(length(fields$prices), meanlog=0, sdlog=.03)
	return(fields)
}

run_gam<-function(formula, fields){
	#runs GAM model and returns coefficient on price
	gam_sim<-gam(formula,
		family=gaussian(link=log), weights=size, data=fields, 
		na.action='na.omit')
	coefficients(gam_sim)
	return(coefficients(gam_sim)["prices"])
}

run_glm<-function(formula, fields){
	glm_sim<-lm(formula, data=fields)
	return(coefficients(glm_sim)["prices"])
}

mc_run<-function(formula, use_gam=TRUE, fields, beta){
	fields_unc<-add_uncertainty(fields=fields, beta=beta)
	if(use_gam){
		return(run_gam(formula, fields_unc))
	}
	else{
		return(run_glm(formula, fields_unc))
	}
}

#****************************************************

oil_price<-read.csv("/Users/johannesmauritzen/research/oil_prices/data/oil_price.csv")

field_size<-round(exp(rnorm(80,mean=2, sd=1)), digits=1)
field_size<-field_size[field_size>5]
init_year<-trunc(sapply(field_size, gen_year))
fields<-cbind(field_size, init_year,1:length(init_year))

#alternatively
#true_data<-read.csv('/Users/johannesmauritzen/research/oil_prices/data/field_data.csv')
#fields<-data.frame(field_size=true_data$recoverable_oil, init_year=)

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
sim_fields_unc<-add_uncertainty(fields=sim_fields, beta=0.05)

simulated_production<-ggplot(sim_fields_unc)+
 geom_line(aes(x=year, y=prod, color=factor(name)),alpha=.3, size=1) +
 guides(color=FALSE) +
 scale_color_grey(start=0, end=.3) +
 theme_bw() +
 labs(x="", y="Simulated Production from Fields")

formula_1<- formula(prod~s(prod_years, name, bs="re") + prices + size + s(year, bs="cr", k=4))
formula_2<- formula(prod~s(prod_years, bs="cr") + prices + size + s(year, bs="cr", k=4))
formula_3<- formula(prod~s(prod_years, bs="cr") + prices + size)
formula_4<- formula(prod~s(prod_years, size) + prices + s(year, bs="cr", k=4))

gam_mc_beta0<-replicate(100, mc_run(beta=0, formula=formula_4, fields=sim_fields, use_gam=TRUE))
gam_mc_beta05<-replicate(100, mc_run(beta=.05, formula=formula_4, fields=sim_fields, use_gam=TRUE))
gam_mc_data<-data.frame(beta0=gam_mc_beta0, beta05=gam_mc_beta05)
#write.csv(gamm_mc_data, "/Users/johannesmauritzen/research/oil_prices/mc_data.csv")
#gam_mc_data<-read.csv("/Users/johannesmauritzen/research/oil_prices/mc_data.csv")
#gam_mc_data$X<-NULL
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










