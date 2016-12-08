# Code to obtain results for thesis
# Created by: Jessica Kim
# Fall 2016
# Runs summary and inferential analysis for thesis

setwd("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_chs")
load("felonies_chs_sales.RData")

############# IDENTIFY GENTRIFIABLE NEIGHBORHOODS ############# 
## Criteria are:
# median household income and median property sales values in the lowest quantile of both distributions across neighborhoods. 
felonies_chs_sales$uhf_pop <- felonies_chs_sales$n
require(dplyr)
felonies_chs_sales2 <- felonies_chs_sales %>%
  mutate(
  	total_sales_uhf_millions  = total_sales_uhf / 1e6,
    median_sales_uhf_millions = median_sales_uhf / 1e6) %>%
  group_by(
  	year, 
  	UHF_CODE) %>%
  mutate(
  	n_grandlarceny 		     = if_else(Offense == "GRAND LARCENY",  n_offense_uhf, as.integer(0)),
    n_rape         		     = if_else(Offense == "RAPE",           n_offense_uhf, as.integer(0)),
    n_burglary     		     = if_else(Offense == "BURGLARY",       n_offense_uhf, as.integer(0)),
    n_assault      		     = if_else(Offense == "FELONY ASSAULT", n_offense_uhf, as.integer(0)),
    n_grandlarceny_vehicle = if_else(Offense == "GRAND LARCENY OF MOTOR VEHICLE", n_offense_uhf, as.integer(0)),
    n_homicide 			       = if_else(Offense == "MURDER & NON-NEGL. MANSLAUGHTE", n_offense_uhf, as.integer(0)),
    n_robbery 	  		     = if_else(Offense == "ROBBERY",        n_offense_uhf, as.integer(0))
    ) %>%
  select(-Offense) %>%
  group_by(
  	year, 
  	UHF_CODE) %>%
  summarise_all(max) %>%
  select(-n_offense_uhf) %>%
  mutate(
  	grandlarceny_per1000 	       = n_grandlarceny / (uhf_pop / 1000),
    rape_per1000 			           = n_rape / (uhf_pop / 1000),
    burglary_per1000 			       = n_burglary / (uhf_pop / 1000),
    assault_per1000 			       = n_assault / (uhf_pop / 1000),
    grandlarceny_vehicle_per1000 = n_grandlarceny_vehicle / (uhf_pop / 1000),
    homicide_per1000 		      	 = n_homicide / (uhf_pop / 1000),
    robbery_per1000 		      	 = n_robbery / (uhf_pop / 1000)
  	) %>%
  group_by(year) %>%
  mutate(
  	medsales_uhf_millions = median(median_sales_uhf_millions),
    medprop_underfpl_uhf  = median(prop_under100fpl)) %>%
  ungroup() %>%
  mutate(
  	gentrifiable = if_else(median_sales_uhf_millions < medsales_uhf_millions 
  	                       & prop_under100fpl > medprop_underfpl_uhf, 1, 0)
  	)

# Summary statistics: look at which neighborhoods were gentrifiable by plotting the neighborhoods that were gentrifiable between 2004 and 2006 on a plot of median sales and then on a plot of poverty proportion
require(ggplot2)
# two lines on same plot of gentrifiable and non gentrifiable neighborhoods 
with(felonies_chs_sales2[felonies_chs_sales2$gentrifiable == 0,],
     plot(year, median_sales_uhf_millions, type = "h", lwd = 3, col = "blue", lty = 3,
          xlim = range(year),
          main = "Median Neighborhood Property \nSales by Gentrification Potential",
          xlab = "Year",
          ylab = "Median Property Sales (Millions of USD)"))
with(felonies_chs_sales2[felonies_chs_sales2$gentrifiable == 1, ], lines(year, median_sales_uhf_millions, col = "red", type = "h", lwd = 3))
legend(x = 2009, y = 2700, legend = c("Gentrifiable", "Not Gentrifiable"), fill = c("blue", "red"), lty = c(3, 1))

with(felonies_chs_sales2[felonies_chs_sales2$gentrifiable == 0,],
     plot(year, assault_per1000, type = "h", lwd = 3, col = "blue", lty = 3,
          xlim = range(year),
          main = "Felony Assault Rate by Gentrification Potential",
          xlab = "Year",
          ylab = "Neighborhood Assaults per 1,000 Residents"))
with(felonies_chs_sales2[felonies_chs_sales2$gentrifiable == 1, ], lines(year, assault_per1000, col = "red", type = "h", lwd = 3))
legend(x = 2004, y = 20, legend = c("Gentrifiable", "Not Gentrifiable"), fill = c("blue", "red"), lty = c(3, 1))

with(felonies_chs_sales2[felonies_chs_sales2$gentrifiable == 0,],
     plot(year, uhf_pop, type = "h", lwd = 3, col = "blue", lty = 3,
          xlim = range(year),
          main = "Felony Assault Rate by Gentrification Potential",
          xlab = "Year",
          ylab = "Neighborhood Assaults per 1,000 Residents"))
with(felonies_chs_sales2[felonies_chs_sales2$gentrifiable == 1, ], lines(year, uhf_pop, col = "red", type = "h", lwd = 3))
legend(x = 2004, y = 20, legend = c("Gentrifiable", "Not Gentrifiable"), fill = c("blue", "red"), lty = c(3, 1))


install.packages("stargazer")
require(stargazer)
vars_for_summary <- as.data.frame(felonies_chs_sales2[, c("prop_black", "prop_foreign", "prop_less_hs", "prop_under100fpl", "total_sales_uhf_millions", "median_sales_uhf_millions", "grandlarceny_per1000", "rape_per1000", "burglary_per1000", "assault_per1000", "grandlarceny_vehicle_per1000", "homicide_per1000", "robbery_per1000", "gentrifiable")])
stargazer(vars_for_summary, summary = TRUE,
          title = c("Summary Statistics"))

## also table of summary statistics for covariates and crime


############# IDENTIFY GENTRIFIED NEIGHBORHOODS ############# 

# Gentrified neighborhoods are ones that experience an increase in average income and education level that is greater than the median of increases across gentrifiable neighborhoods.\\
# 



############# FIT BAYESIAN MODEL ############# 
# library(rstan)
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
# 
# # Define MCMC parameters 
# niter <- 1E4   # definitely overkill, but good for comparison
# nchains <- 4