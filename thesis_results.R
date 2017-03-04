# Code to obtain results for thesis
# Created by: Jessica Kim
# Fall 2016
# Runs summary and inferential analysis for thesis

setwd("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_chs")
load("felonies_chs_sales.RData")
load("thesis_12_17.RData")
############# IDENTIFY GENTRIFIABLE NEIGHBORHOODS ############# 
## Criteria are:
# median household income and median property sales values in the lowest quantile of both distributions across neighborhoods. 
felonies_chs_sales$uhf_pop <- felonies_chs_sales$n
require(dplyr)
felonies_chs_sales_final <- felonies_chs_sales %>%
  mutate(
  	total_sales_uhf_millions  = total_sales_uhf / 1e6,
    median_sales_uhf_millions = median_sales_uhf / 1e6
  	) %>%
  group_by(year, UHF_CODE) %>%
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
  group_by(year, UHF_CODE) %>%
  summarise_all(max) %>%
  mutate(
    felonies_per1000             = n_felonies_uhf / (uhf_pop / 1000),
  	grandlarceny_per1000 	       = n_grandlarceny / (uhf_pop / 1000),
    rape_per1000 			           = n_rape / (uhf_pop / 1000),
    burglary_per1000 			       = n_burglary / (uhf_pop / 1000),
    assault_per1000 			       = n_assault / (uhf_pop / 1000),
    grandlarceny_vehicle_per1000 = n_grandlarceny_vehicle / (uhf_pop / 1000),
    homicide_per1000 		      	 = n_homicide / (uhf_pop / 1000),
    robbery_per1000 		      	 = n_robbery / (uhf_pop / 1000),
    property_felonies_per1000    = (n_grandlarceny + n_burglary + n_grandlarceny_vehicle + n_robbery) / (uhf_pop / 1000),
    violent_felonies_per1000     = (n_rape + n_assault + n_homicide) / (uhf_pop / 1000)
  	) %>%
  arrange(year) %>%
  mutate(
  	medsales_alluhf_millions = median(median_sales_uhf_millions),
    medprop_underfpl_alluhf  = median(prop_under100fpl)
  	) %>%
  group_by(UHF_CODE) %>%
  arrange(year) %>%
  mutate(
    pctchg_median_sales_uhf_millions = round( (median_sales_uhf_millions - lag(median_sales_uhf_millions))
                                              / lag(median_sales_uhf_millions) * 100, 3),
    pctchg_prop_underfpl_uhf         = round( (prop_under100fpl - lag(prop_under100fpl))
                                              / lag(prop_under100fpl) * 100, 3)
    ) %>%
  group_by(year) %>%
  mutate(
    median_pctchg_median_sales = median(pctchg_median_sales_uhf_millions),
    median_pctchg_prop_underfpl = median(pctchg_prop_underfpl_uhf)
  ) %>%
  ungroup() %>%
  mutate(
  	pre_gent = if_else(median_sales_uhf_millions < medsales_alluhf_millions 
  	                       & prop_under100fpl > medprop_underfpl_alluhf 
  	                       & year %in% c(2003:2006), 1, 0)
  ) %>%
  group_by(UHF_CODE) %>%
  arrange(year) %>%
  mutate(
  	gentrifiable = if_else(pre_gent == 1
  	                       | lag(pre_gent) == 1
  	                       | lag(pre_gent, 2L) == 1
  	                       | lag(pre_gent, 3L) == 1
  	                       | lag(pre_gent, 4L) == 1
  	                       | lag(pre_gent, 5L) == 1
  	                       | lag(pre_gent, 6L) == 1
  	                       | lag(pre_gent, 7L) == 1
  	                       | lag(pre_gent, 8L) == 1
  	                       | lag(pre_gent, 9L) == 1
  	                       | lag(pre_gent, 10L) == 1
  	                       | lead(pre_gent) == 1
  	                       | lead(pre_gent, 2L) == 1
  	                       | lead(pre_gent, 3L) == 1
  	                       , 1, 0, 0),
  	# Gentrified neighborhoods are ones that experience an increase in average income and education level that is greater than the median of increases across gentrifiable neighborhoods.
  	pre_gentrified   = if_else(pctchg_median_sales_uhf_millions > median_pctchg_median_sales
  	                       & pctchg_prop_underfpl_uhf < median_pctchg_prop_underfpl
  	                       & year %in% c(2007:2014)
  	                       & gentrifiable == 1, 1, 0),
  	gentrified = if_else(pre_gentrified == 1
  	                     | lag(pre_gentrified) == 1
  	                     | lag(pre_gentrified, 2L) == 1
  	                     | lag(pre_gentrified, 3L) == 1
  	                     | lag(pre_gentrified, 4L) == 1
  	                     | lag(pre_gentrified, 5L) == 1
  	                     | lag(pre_gentrified, 6L) == 1,
  	                     1, 0, 0)
  	)


# Summary statistics: look at which neighborhoods were gentrifiable by plotting the neighborhoods that were gentrifiable between 2004 and 2006 on a plot of median sales and then on a plot of poverty proportion
require(ggplot2)
# two lines on same plot of gentrifiable and non gentrifiable neighborhoods
require(tikzDevice)
require(lattice)
tikz('plot_sales_gentrifiable.tex', height = 6, width = 6)
barchart(as.factor(year) ~ median_sales, groups = gent_bin, gentrifiable_table, 
         auto.key = list(columns = 2), 
         main = "Median Neighborhood Property \nSales By Gentrification Potential", 
         xlab = "Median Property Sales (Millions of USD)",
         ylab = "Year",
         par.settings = list(superpose.polygon = list(col = c('blue', 'red'))))
dev.off()
tikz('plot_poor_gentrifiable.tex', height = 6, width = 6)
barchart(as.factor(year) ~ median_poor, groups = gent_bin, gentrifiable_table, 
         auto.key = list(columns = 2), 
         main = "Proportion of Residents At Or Below FPL\n By Gentrification Potential", 
         xlab = "Proportion of Neighborhood Residents",
         ylab = "Year",
         par.settings = list(superpose.polygon = list(col = c('steelblue', 'red'))))
dev.off()

with(felonies_chs_sales_final[felonies_chs_sales_final$gentrifiable == 0,],
     plot(year, median_sales_uhf_millions, type = "h", lwd = 5, col = "blue",
          xlim = c(2004, 2006),
          lab = c(2, 7, 7),
          ylim = c(0, max(median_sales_uhf_millions) + 300),
          main = "Proportion of Residents <= 100 FPL\n by Gentrification Potential",
          xlab = "Year",
          ylab = "Proportion of Neighborhood Residents"))

with(felonies_chs_sales_final[felonies_chs_sales_final$gentrifiable == 1, ], lines(year, median_sales_uhf_millions, col = "red", type = "h", lwd = 15))
legend(x = 2004, y = 2700, legend = c("Not Gentrifiable", "Gentrifiable"), lwd = c(5, 15),  col = c("blue", "red"), bty = "n")
dev.off()

# with(felonies_chs_sales_final[felonies_chs_sales_final$gentrifiable == 1,],
#      plot(year, prop_under100fpl, type = "h", lwd = 15, col = "red",
#           xlim = range(year),
#           ylim = c(0, max(prop_under100fpl) + 10),
#           main = "Proportion of Residents <= 100 FPL\n by Gentrification Potential",
#           xlab = "Year",
#           ylab = "Proportion of Neighborhood Residents"))
# with(felonies_chs_sales_final[felonies_chs_sales_final$gentrifiable == 0, ], lines(year, prop_under100fpl, col = "black", type = "h", lwd = 5))
# legend(x = 2004, y = 50, legend = c("Not Gentrifiable", "Gentrifiable"), lwd = c(5, 15), col = c("steelblue4", "red"), bty = "n")
# dev.off()
# 
# with(felonies_chs_sales_final[felonies_chs_sales_final$gentrifiable == 0,],
#      plot(year, uhf_pop, type = "h", lwd = 3, col = "blue", lty = 3,
#           xlim = range(year),
#           main = "Felony Assault Rate by Gentrification Potential",
#           xlab = "Year",
#           ylab = "Neighborhood Assaults per 1,000 Residents"))
# with(felonies_chs_sales_final[felonies_chs_sales_final$gentrifiable == 1, ], lines(year, uhf_pop, col = "red", type = "h", lwd = 3))
# legend(x = 2004, y = 20, legend = c("Gentrifiable", "Not Gentrifiable"), fill = c("blue", "red"), lty = c(3, 1))

## table of summary statistics for covariates and crime
require(stargazer)
vars_for_summary <- as.data.frame(felonies_chs_sales_final[, c("prop_black", "prop_foreign", "prop_less_hs", "prop_under100fpl", "total_sales_uhf_millions", "median_sales_uhf_millions", "pctchg_median_sales_uhf_millions", "pctchg_prop_underfpl_uhf", "felonies_per1000", "property_felonies_per1000", "violent_felonies_per1000")])
stargazer(vars_for_summary, summary = TRUE, title = c("Summary Statistics"), digits = 1, omit.summary.stat = c("n"))

vars_for_gent_summary <- as.data.frame(felonies_chs_sales_final[felonies_chs_sales_final$gentrifiable == 1, c("prop_black", "prop_foreign", "prop_less_hs", "prop_under100fpl", "total_sales_uhf_millions", "median_sales_uhf_millions", "pctchg_median_sales_uhf_millions", "pctchg_prop_underfpl_uhf", "grandlarceny_per1000", "rape_per1000", "burglary_per1000", "assault_per1000", "grandlarceny_vehicle_per1000", "homicide_per1000", "robbery_per1000")])
stargazer(vars_for_gent_summary, summary = TRUE, title = c("Summary Statistics for Gentrifiable Neighborhoods"), digits = 1, omit.summary.stat = c("n"))

## plot separate boxplots for each neighborhood to see if random effects are needed for outcome


dev.off()

df <- data.frame(id = c(rep("Good",200), rep("Bad", 200)),
                 F1 = c(rnorm(200,10,2), rnorm(200,8,1)),
                 F2 = c(rnorm(200,7,1),  rnorm(200,6,1)),
                 F3 = c(rnorm(200,6,2),  rnorm(200,9,3)),
                 F4 = c(rnorm(200,12,3), rnorm(200,8,2)))

boxplot(df[,-1], xlim = c(0.5, ncol(df[,-1])+0.5), 
        boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1)) #invisible boxes
boxplot(df[which(df$id=="Good"), -1], xaxt = "n", add = TRUE, boxfill="red", boxwex=0.25, 
        at = 1:ncol(df[,-1]) - 0.15) #shift these left by -0.15
boxplot(df[which(df$id=="Bad"), -1], xaxt = "n", add = TRUE, boxfill="blue", boxwex=0.25,
        at = 1:ncol(df[,-1]) + 0.15) #shift these right by +0.15


boxplot(felonies_chs_sales_final$robbery_per1000, add = TRUE)





############# FIT BAYESIAN MODEL ############# 
# library(rstan)
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
# 
# # Define MCMC parameters 
# niter <- 1E4   # definitely overkill, but good for comparison
# nchains <- 4

# PRE DATA STEPS
1. Have one or more theories for how data are generated 
# We are estimating the probability of the effect of gentrification on crime rate. I will use a standard normal prior distribution as the literature suggests that there is a possibility for a negative effect.
require(rstanarm)
require(lme4)
require(brms)
require(bayesplot)
require(loo)
CHAINS <- parallel::detectCores()
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# allow for random intercept and interact the effect of gentrified with whether the neighborhood was gentrifiable. Account for time effect by including the year variable. Account for neighborhood heterogeneity by allowing the intercept to vary by neighborhood.

# The effect has a 95% probability of being between 0.1 and 1.7 (the credible interval)

# "We specify QR = TRUE for performance reasons when we do not want to specify an informative prior on the common coefficients. In addition, we specify init_r = 1 to get initial values on the (−1,1)(−1,1) interval in the unconstrained space, which works better in this case." BEN GOODRICH

felonies_chs_sales_final$Gentrified = as.factor(felonies_chs_sales_final$gentrified)
felonies_chs_sales_final$Gentrifiable = as.factor(felonies_chs_sales_final$gentrifiable)
felonies_chs_sales_final$logfelonies_per1000 = log(felonies_chs_sales_final$felonies_per1000)

stanlmm1_alloffense <- stan_lmer(
  formula = felonies_per1000 ~ Gentrified * Gentrifiable + (year|UHF_CODE), 
  data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)
bayesplot::color_scheme_set("pink")
(trace <- plot(stanlmm1_alloffense, "trace", pars = "(Intercept)"))
(trace <- plot(stanlmm1_alloffense, "trace", pars = "as.factor(gentrified)1"))
(trace <- plot(stanlmm1_alloffense, "trace", pars = "as.factor(gentrifiable)1"))
stanlmm1_alloffense_summary <- summary(stanlmm1_alloffense, pars = c("(Intercept)", "Gentrified1", "Gentrifiable1"), probs = c(0.05, 0.95))
stargazer(stanlmm1_alloffense_summary, digits = 2)
alloffense_ci90 <- round(posterior_interval(stanlmm1_alloffense, pars = c("(Intercept)", "Gentrified1", "Gentrifiable1")), 2)
stargazer(alloffense_ci90, digits = 2)

plot_alloffense_restricted <- plot(stanlmm1_alloffense, pars = c("Gentrified1", "Gentrifiable1")) +
  ggtitle("LMM Restricted Model Predicting\n Felonies per 1,000") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 35, by = 5), 5), limits = c(-25, 35))
loo_lmm1_alloffense <- loo(stanlmm1_alloffense) # leave one out cross validation

felonies_chs_sales_final <- felonies_chs_sales_final %>%
  mutate(propmale = prop_male,
         propforeign = prop_foreign,
         propblack = prop_black)
stanlmm2_alloffense <- stan_lmer(
  formula = felonies_per1000 ~ Gentrified * Gentrifiable + propmale + propforeign + propblack + (year | UHF_CODE),
  data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)

stanlmm2_alloffense_summary <- summary(stanlmm2_alloffense, probs = c(0.05, 0.95), pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack"))
stargazer(stanlmm2_alloffense_summary, digits = 2, title = "LMM Unrestricted Model Predicting \nTreatment Effect on Felonies per 1,000")
plot_alloffense_unrestricted <- plot(stanlmm2_alloffense, 
                                     pars = c("(Intercept)", "Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack")) +
  ggtitle("Unrestricted Model") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 35, by = 5), 5), limits = c(-25, 35))

loo_lmm2_alloffense <- loo(stanlmm2_alloffense)
alloffense_ci90_unrestrict <- round(posterior_interval(stanlmm2_alloffense, prob = 0.90, pars = c("Gentrified1", "Gentrifiable1", "prop_male", "prop_foreign", "prop_black")), 2)


felonies_chs_sales_final <- felonies_chs_sales_final %>%
  mutate(year20082009 = ifelse(year == 2008 | year == 2009, 1, 0))
stanlmm3_alloffense <- stan_lmer(formula = felonies_per1000 ~ Gentrified * Gentrifiable + propmale + propforeign + propblack + year20082009 + (year | UHF_CODE), data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)
stanlmm3_alloffense_summary <- summary(stanlmm3_alloffense, probs = c(0.05, 0.95), pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009"))
stargazer(stanlmm3_alloffense_summary, title = "LMM Unrestricted Model 2 Predicting \nTreatment Effect on Felonies per 1,000", digits = 2)

plot_alloffense_unrestricted2 <- plot(stanlmm3_alloffense, 
                                     pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009")) +
  ggtitle("Unrestricted Model with Recession Indicator") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 30, by = 5), 5), limits = c(-25, 30))
alloffense_ci90_unrestrict2 <- round(posterior_interval(stanlmm3_alloffense, prob = 0.90, pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009")), 2)
stargazer(alloffense_ci90_unrestrict2)

loo_lmm3_alloffense <- loo(stanlmm3_alloffense)

# compare simulated data from posterior predictive distribution with actual response values
tikz('plot_alloffense2_ppcheck1.tex', width = 3, height = 3)
pp_check(stanlmm1_alloffense) + 
  ggtitle("Posterior Predictive Check\nLMM Restricted Model Predicting Felonies per 1,000")
dev.off()
tikz('plot_alloffense2_ppcheck2.tex', width = 3, height = 3)
pp_check(stanlmm2_alloffense) +
  ggtitle("Posterior Predictive Check\nLMM Unrestricted Model Predicting Felonies per 1,000")
dev.off()
tikz('plot_alloffense2_ppcheck3.tex', width = 3, height = 3)
pp_check(stanlmm3_alloffense) +
  ggtitle("Posterior Predictive Check LMM Unrestricted Model\nWith Recession Indicator Predicting Felonies per 1,000")
dev.off()


stargazer(compare(loo_lmm1_alloffense, loo_lmm2_alloffense, loo_lmm3_alloffense), digits = 2)

tikz('plot_alloffense1_coefs.tex', width = 5, height = 2)
plot_alloffense_restricted
dev.off()
# devtools::install_github("yihui/tikzDevice", force = TRUE)
tikz('plot_alloffense2_coefs.tex', width = 5, height = 2)
plot_alloffense_unrestricted
dev.off()
tikz('plot_alloffense3_coefs.tex', width = 5, height = 2)
plot_alloffense_unrestricted2
dev.off()
# save.image(file = "thesis_12_22.RData")
# load("thesis_12_22.RData")

# Structural equation model
# evaluates the simultaneous causality of gentrification and felony incident rates
# 
# library(blavaan)
# require(rjags)
# require(lavaan)
# alloffense_sem <- '
# # regressions
# Gentrified ~ propmale + propblack + propforeign + year20082009
# felonies_per1000 ~ propmale + propblack + propforeign + year20082009
# # residual correlations
# Gentrified ~ felonies_per1000
# '
# ## unique priors for mv intercepts; parallel chains
# alloffense_sem_fit <- bsem(alloffense_sem, data=felonies_chs_sales_final, test = "none",
#                            jagcontrol=list(method="rjparallel"))
# summary(alloffense_sem_fit, standardized = TRUE, fit.measures = TRUE)


########### property crimes ##############

stanlmm1_property <- stan_lmer(
  formula = property_felonies_per1000 ~ Gentrified * Gentrifiable + (year|UHF_CODE), 
  data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)
bayesplot::color_scheme_set("pink")
(trace <- plot(stanlmm1_property, "trace", pars = "(Intercept)"))
(trace <- plot(stanlmm1_property, "trace", pars = "as.factor(gentrified)1"))
(trace <- plot(stanlmm1_property, "trace", pars = "as.factor(gentrifiable)1"))
stanlmm1_property_summary <- summary(stanlmm1_property, pars = c("(Intercept)", "Gentrified1", "Gentrifiable1"), probs = c(0.05, 0.95))
stargazer(stanlmm1_property_summary, digits = 2)
property_ci90 <- round(posterior_interval(stanlmm1_property, pars = c("(Intercept)", "Gentrified1", "Gentrifiable1")), 2)
stargazer(property_ci90, digits = 2)

plot_property_restricted <- plot(stanlmm1_property, pars = c("Gentrified1", "Gentrifiable1")) +
  ggtitle("LMM Restricted Model Predicting \nProperty Felonies per 1,000") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 35, by = 5), 5), limits = c(-25, 35))
loo_lmm1_property <- loo(stanlmm1_property) # leave one out cross validation

felonies_chs_sales_final <- felonies_chs_sales_final %>%
  mutate(propmale = prop_male,
         propforeign = prop_foreign,
         propblack = prop_black)
stanlmm2_property <- stan_lmer(
  formula = property_felonies_per1000 ~ Gentrified * Gentrifiable + propmale + propforeign + propblack + (year | UHF_CODE),
  data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)

stanlmm2_property_summary <- summary(stanlmm2_property, probs = c(0.05, 0.95), pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack"))
stargazer(stanlmm2_property_summary, digits = 2, title = "Unrestricted Model Predicting \nTreatment Effect on Property Felonies per 1,000")
plot_property_unrestricted <- plot(stanlmm2_property, 
                                     pars = c("(Intercept)", "Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack")) +
  ggtitle("Unrestricted Model") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 35, by = 5), 5), limits = c(-25, 35))

loo_lmm2_property <- loo(stanlmm2_property)
property_ci90_unrestrict <- round(posterior_interval(stanlmm2_property, prob = 0.90, pars = c("Gentrified1", "Gentrifiable1", "prop_male", "prop_foreign", "prop_black")), 2)


felonies_chs_sales_final <- felonies_chs_sales_final %>%
  mutate(year20082009 = ifelse(year == 2008 | year == 2009, 1, 0))
stanlmm3_property <- stan_lmer(formula = property_felonies_per1000 ~ Gentrified * Gentrifiable + propmale + propforeign + propblack + year20082009 + (year | UHF_CODE), data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)
stanlmm3_property_summary <- summary(stanlmm3_property, probs = c(0.05, 0.95), pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009"))
stargazer(stanlmm3_property_summary, title = "LMM Unrestricted Model 2 Predicting \nTreatment Effect on Property Felonies per 1,000", digits = 2)

plot_property_unrestricted2 <- plot(stanlmm3_property, 
                                      pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009")) +
  ggtitle("Unrestricted Model with Recession Indicator") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 30, by = 5), 5), limits = c(-25, 30))
property_ci90_unrestrict2 <- round(posterior_interval(stanlmm3_property, prob = 0.90, pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009")), 2)
stargazer(property_ci90_unrestrict2)

loo_lmm3_property <- loo(stanlmm3_property)

# compare simulated data from posterior predictive distribution with actual response values
tikz('plot_property2_ppcheck1.tex', width = 3, height = 3)
pp_check(stanlmm1_property) + 
  ggtitle("Posterior Predictive Check\nLMM Restricted Model Predicting Property Felonies per 1,000")
dev.off()
tikz('plot_property2_ppcheck2.tex', width = 3, height = 3)
pp_check(stanlmm2_property) +
  ggtitle("Posterior Predictive Check\nLMM Unrestricted Model Predicting Property Felonies per 1,000")
dev.off()
tikz('plot_property2_ppcheck3.tex', width = 3, height = 3)
pp_check(stanlmm3_property) +
  ggtitle("Posterior Predictive Check LMM Unrestricted Model\nWith Recession Indicator Predicting Property Felonies per 1,000")
dev.off()


stargazer(compare(loo_lmm1_property, loo_lmm2_property, loo_lmm3_property), digits = 2)

tikz('plot_property1_coefs.tex', width = 5, height = 2)
plot_property_restricted
dev.off()
# devtools::install_github("yihui/tikzDevice", force = TRUE)
tikz('plot_property2_coefs.tex', width = 5, height = 2)
plot_property_unrestricted
dev.off()
tikz('plot_property3_coefs.tex', width = 5, height = 2)
plot_property_unrestricted2
dev.off()


########### violent crimes ##############

stanlmm1_violent <- stan_lmer(
  formula = violent_felonies_per1000 ~ Gentrified * Gentrifiable + (year|UHF_CODE), 
  data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)
bayesplot::color_scheme_set("pink")
(trace <- plot(stanlmm1_violent, "trace", pars = "(Intercept)"))
(trace <- plot(stanlmm1_violent, "trace", pars = "as.factor(gentrified)1"))
(trace <- plot(stanlmm1_violent, "trace", pars = "as.factor(gentrifiable)1"))
stanlmm1_violent_summary <- summary(stanlmm1_violent, pars = c("(Intercept)", "Gentrified1", "Gentrifiable1"), probs = c(0.05, 0.95))
stargazer(stanlmm1_violent_summary, digits = 2)
violent_ci90 <- round(posterior_interval(stanlmm1_violent, pars = c("(Intercept)", "Gentrified1", "Gentrifiable1")), 2)
stargazer(violent_ci90, digits = 2)

plot_violent_restricted <- plot(stanlmm1_violent, pars = c("Gentrified1", "Gentrifiable1")) +
  ggtitle("LMM Restricted Model Predicting \nViolent Felonies per 1,000") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 35, by = 5), 5), limits = c(-25, 35))
loo_lmm1_violent <- loo(stanlmm1_violent) # leave one out cross validation

felonies_chs_sales_final <- felonies_chs_sales_final %>%
  mutate(propmale = prop_male,
         propforeign = prop_foreign,
         propblack = prop_black)
stanlmm2_violent <- stan_lmer(
  formula = violent_felonies_per1000 ~ Gentrified * Gentrifiable + propmale + propforeign + propblack + (year | UHF_CODE),
  data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)

stanlmm2_violent_summary <- summary(stanlmm2_violent, probs = c(0.05, 0.95), pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack"))
stargazer(stanlmm2_violent_summary, digits = 2, title = "LMM Unrestricted Model Predicting \nTreatment Effect on Violent Felonies per 1,000")
plot_violent_unrestricted <- plot(stanlmm2_violent, 
                                     pars = c("(Intercept)", "Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack")) +
  ggtitle("Unrestricted Model") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 35, by = 5), 5), limits = c(-25, 35))

loo_lmm2_violent <- loo(stanlmm2_violent)
violent_ci90_unrestrict <- round(posterior_interval(stanlmm2_violent, prob = 0.90, pars = c("Gentrified1", "Gentrifiable1", "prop_male", "prop_foreign", "prop_black")), 2)


felonies_chs_sales_final <- felonies_chs_sales_final %>%
  mutate(year20082009 = ifelse(year == 2008 | year == 2009, 1, 0))
stanlmm3_violent <- stan_lmer(formula = violent_felonies_per1000 ~ Gentrified * Gentrifiable + propmale + propforeign + propblack + year20082009 + (year | UHF_CODE), data = felonies_chs_sales_final, chains = CHAINS, seed = 1234, QR = TRUE)
stanlmm3_violent_summary <- summary(stanlmm3_violent, probs = c(0.05, 0.95), pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009"))
stargazer(stanlmm3_violent_summary, title = "LMM Unrestricted Model 2 Predicting \nTreatment Effect on Violent Felonies per 1,000", digits = 2)

plot_violent_unrestricted2 <- plot(stanlmm3_violent, 
                                      pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009")) +
  ggtitle("Unrestricted Model with Recession Indicator") +
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = round(seq(-25, 30, by = 5), 5), limits = c(-25, 30))
violent_ci90_unrestrict2 <- round(posterior_interval(stanlmm3_violent, prob = 0.90, pars = c("Gentrified1", "Gentrifiable1", "propmale", "propforeign", "propblack", "year20082009")), 2)
stargazer(violent_ci90_unrestrict2)

loo_lmm3_violent <- loo(stanlmm3_violent)

# compare simulated data from posterior predictive distribution with actual response values
tikz('plot_violent2_ppcheck1.tex', width = 3, height = 3)
pp_check(stanlmm1_violent) + 
  ggtitle("Posterior Predictive Check\nLMM Restricted Model Predicting Property Felonies per 1,000")
dev.off()
tikz('plot_violent2_ppcheck2.tex', width = 3, height = 3)
pp_check(stanlmm2_violent) +
  ggtitle("Posterior Predictive Check\nLMM Unrestricted Model Predicting Property Felonies per 1,000")
dev.off()
tikz('plot_violent2_ppcheck3.tex', width = 3, height = 3)
pp_check(stanlmm3_violent) +
  ggtitle("Posterior Predictive Check LMM Unrestricted Model\nWith Recession Indicator Predicting Property Felonies per 1,000")
dev.off()


stargazer(compare(loo_lmm1_violent, loo_lmm2_violent, loo_lmm3_violent), digits = 2)

tikz('plot_violent1_coefs.tex', width = 5, height = 2)
plot_violent_restricted
dev.off()
# devtools::install_github("yihui/tikzDevice", force = TRUE)
tikz('plot_violent2_coefs.tex', width = 5, height = 2)
plot_violent_unrestricted
dev.off()
tikz('plot_violent3_coefs.tex', width = 5, height = 2)
plot_violent_unrestricted2
dev.off()
save.image(file = "thesis_12_27.RData")
load("thesis_12_27.RData")


# put data into list

felonies_chs_sales_list <- list(felonies_chs_sales_final)

# subset group

# read in data:
rDat <- read.table("gibsonwu2012data.txt", header = TRUE)
# subset critical region:
rDat <- subset(rDat, region == "headnoun")
# create data as list for Stan, and fit model:
stanDat <- list(rt = rDat$rt, so = rDat$type, N = nrow(rDat))
library(stan)
fixEfFit <- stan(file = "fixEf.stan", data = stanDat,
                 iter = 2000, chains = 4)
# plot traceplot, excluding warm-up:
traceplot(fixEfFit, pars = c("beta", "sigma_e"),
          inc_warmup = FALSE)
# examine quantiles of posterior distributions:
print(fixEfFit, pars = c("beta", "sigma_e"),
      probs = c(0.025, 0.5, 0.975))
# examine quantiles of parameter of interest:
beta1 <- unlist(extract(fixEfFit, pars = "beta[2]"))
print(quantile(beta1, probs = c(0.025, 0.5, 0.975)))


## webscrape census data for urban population in US cities
require(rvest)
url <- "https://en.wikipedia.org/wiki/Urbanization_in_the_United_States"
urban_props_us <- url %>%
read_html() %>%
html_table(header = FALSE)
urban_props_us <- urban_props_us[[1]]
colnames(urban_props_us) <- urban_props_us[2, ]
urban_props_us <- urban_props_us[-1, ]
urban_props_us_1960_to_2010 <- urban_props_us[, 1:7]
years <- colnames(urban_props_us_1960_to_2010[, -1])
urban_props_us_1960_to_2010_long <- reshape(urban_props_us_1960_to_2010, 
                                            varying = years,
                                            v.names = "state_territory",
                                            timevar = "year",
                                            times = years,
                                            # new.row.names = 1:7,
                                            direction = "long")
urban_props_us_1960_to_2010_long_order <- urban_props_us_1960_to_2010_long[order(urban_props_us_1960_to_2010_long$`State/Territory`),]
urban_props_us_1960_to_2010_long_order$props <- gsub("%", "", urban_props_us_1960_to_2010_long_order$state_territory)

tikz(file = "prop_urban_plot", width = 5, height = 5)
with(subset(urban_props_us_1960_to_2010_long_order, grepl("*United*", `State/Territory`)),
     plot(year, as.numeric(props),
          main = "Proportion of Population Living in Urban Area\nby Region, State, and U.S. Overall",
          ylab = "Percentage of Total Population",
          xlab = "Year",
          ylim = c(0, 100),
          type = "l"))
mtext("Source: U.S. Census Bureau", side = 1, line = 4, adj = 0)
with(subset(urban_props_us_1960_to_2010_long_order, grepl("*east*", `State/Territory`)),
     lines(year, as.numeric(props), type = "l", lty = 2))
with(subset(urban_props_us_1960_to_2010_long_order, grepl("*mid*", `State/Territory`)),
     lines(year, as.numeric(props), type = "l", lty = 3))
with(subset(urban_props_us_1960_to_2010_long_order, `State/Territory` == "South[1]"),
     lines(year, as.numeric(props), type = "l", lty = 4))
with(subset(urban_props_us_1960_to_2010_long_order, `State/Territory` == "West[1]"),
     lines(year, as.numeric(props), type = "l", lty = 5))
with(subset(urban_props_us_1960_to_2010_long_order, grepl("*York*", `State/Territory`)),
     lines(year, as.numeric(props), type = "l", lty = 6, lwd = 3))
legend(1970, 40, c("U.S.", "East", "Midwest", "South", "West", "New York"), lty = c(1, 2, 3, 4, 5, 6), lwd = c(1, 1, 1, 1, 1, 3))
dev.off()


## Citations

## the basic R reference
citation()

if(nchar(system.file(package = "dplyr"))) citation("dplyr")
if(nchar(system.file(package = "rstanarm"))) citation("rstanarm")
if(nchar(system.file(package = "tikzDevice"))) citation("tikzDevice")
if(nchar(system.file(package = "lattice"))) citation("lattice")
if(nchar(system.file(package = "loo"))) citation("loo")


     