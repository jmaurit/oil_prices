#rr_descriptives.r
#last updated March 24th

library(ggplot2)
library(dplyr)
library(reshape2)
library(mgcv)
library(lubridate) 
library(grid)
#library(arm)
library(texreg)
library(zoo)

field_data<-read.csv('/Users/johannesmauritzen/research/oil_prices/data/field_data.csv')
prod_data<-read.csv('/Users/johannesmauritzen/research/oil_prices/data/prod_data.csv')

#How much of the total oil reserves are we excluding
tot_rec_oil<-sum(field_data["recoverable_oil"])
small_rec_oil<-sum(field_data["recoverable_oil"][field_data["recoverable_oil"]<=3])
28.3/4779.2
#.5% of total recoverable oil.  


#plot of investing
#from ssb
invest <- read.csv("http://data.ssb.no/api/v0/dataset/166334.csv?lang=no", sep=";")
colnames(invest) <- c("type", "period", "var", "investment")
invest <- reshape(invest, 
  timevar = "type",
  idvar = c("period", "var"),
  direction = "wide")

quarter <- as.Date(as.yearqtr(invest$period, format = "%YK%q"))
invest$period <- quarter
invest <- invest[c("period", "investment.0 Investeringer i alt", 
	"investment.10.3.3 Produksjonsboring, felt i drift",
	"investment.10.3 Felt i drift",
	"investment.10.2 Feltutbygging", 
	"investment.10.1 Leting og konseptstudier",
	"investment.10.1.1 Unders\xf8kelsesboring")]


invest <- invest[invest$period<as.Date("2015-01-01"),]

colnames(invest) <-c("period", "Total", 
	"In Production, Drilling", "In Production, Total", "Build-out", "Search", "Expl. Drilling")

invest_long <- melt(invest, id.vars="period")

investment<-ggplot(invest_long, aes(x=period, y=value, color=variable)) +
#facet_wrap(nrow=2, ~variable, scales="free_y") +
geom_line() +
scale_color_grey() +
theme_bw() +
#geom_text() +
annotate("text", x = as.Date("2016-01-01"), y = 50000, label = "Total") +
annotate("text", x = as.Date("2016-06-01"), y = 21000, label = "In Production, Total") +
annotate("text", x = as.Date("2016-01-01"), y = 19000, label = "Build-out") +
annotate("text", x = as.Date("2016-06-01"), y = 11000, label = "In Production Drilling") +
annotate("text", x = as.Date("2016-01-01"), y = 8000, label = "Search") +
annotate("text", x = as.Date("2016-06-01"), y = 6000, label = "Expl. Drilling") +
labs(x="", y="Investment, Mill NOK, Seasonally Adjusted") +
xlim(as.Date("2000-01-01"), as.Date("2017-06-01")) +
guides(color=FALSE) 

png("/Users/johannesmauritzen/research/oil_prices/figures/investment.png", 
	width = 25, height = 18, units = "cm", res=600, pointsize=12)
investment
dev.off()

top10<-as.character(head(field_data$name[order(field_data$recoverable_oil, decreasing=TRUE)],10))

#function to to show field-level of data
fields_lim<-subset(prod_data, name %in% top10)
fields_lim<-fields_lim[,c("name", "year", "oil_prod_mill_sm3")]
fields_lim_long<-melt(fields_lim, id.vars=c("name", "year", "oil_prod_mill_sm3"))

top10_production<-ggplot(fields_lim_long, aes(x=year, y=oil_prod_mill_sm3, color=name)) +
geom_line() +
scale_color_grey() +
theme_bw() +
labs(x="", y="Yearly Oil Production, Mill SM3", color="Field") +
guides(color=FALSE)


#total production
prod_grouped<-group_by(prod_data, year)
tot_year_prod<-summarise(prod_grouped, tot_year_prod=sum(oil_prod_mill_sm3, na.rm=TRUE))

#plot total production with oil price
oil_decline<-ggplot(tot_year_prod) +
geom_line(aes(x=year, y=tot_year_prod)) +
xlab("") + ylab("Total Norwegian Production of Oil, Mill SM3") +
theme_bw()


png("/Users/johannesmauritzen/research/oil_prices/figures/oil_decline.png", 
	width = 25, height = 20, units = "cm", res=600, pointsize=12)
grid.newpage()
	pushViewport(viewport(layout = grid.layout(2, 1)))
	vplayout <- function(x, y)
	  viewport(layout.pos.row = x, layout.pos.col = y)
	print(top10_production, vp = vplayout(1, 1))
	print(oil_decline, vp = vplayout(2, 1))
dev.off()

#cost index and oil prices
cost_index<-read.csv("/Users/johannesmauritzen/research/oil_prices/data/cost_index.csv")

cost_long<-melt(cost_index[,c(2,5,6)], id.vars="year")
cost_long<-cost_long[cost_long$year<2015,]

levels(cost_long$variable) <- c("Cost Index, Oil and Gas Extraction", "Real Oil Price, USD (2010)")

cost_index_plot<-ggplot(cost_long) + 
geom_line(aes(x=year, y = value)) +
facet_wrap(nrow=2, ~variable, scales="free_y") +
theme_bw() +
scale_color_grey() +
labs(x="", y="", title="Production Cost Index and Brent Oil Price")

#Field size

size_long<-melt(field_data[,c("in_place_oil_mill_sm3", "recoverable_oil")],)

levels(size_long$variable)<-c("In-place Oil", "Recoverable Oil")

oil_size_plot<-ggplot(size_long, aes(x=value)) +
geom_histogram() +
theme_bw() +
facet_wrap(nrow=2, ~variable) +
labs(x="Oil, Mill. SM3", y="", title="Histogram of Field Size")

png("/Users/johannesmauritzen/research/oil_prices/figures/prod_costs.png", 
	width = 25, height = 18, units = "cm", res=600, pointsize=12)
cost_index_plot
dev.off()

png("/Users/johannesmauritzen/research/oil_prices/figures/data_descriptives.png", 
	width = 25, height = 18, units = "cm", res=600, pointsize=12)
grid.newpage()
	pushViewport(viewport(layout = grid.layout(1, 2)))
	vplayout <- function(x, y)
	  viewport(layout.pos.row = x, layout.pos.col = y)
	print(oil_size_plot, vp = vplayout(1, 1))
	print(cost_index_plot, vp = vplayout(1, 2))
dev.off()

