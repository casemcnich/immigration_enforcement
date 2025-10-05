##########################################
# running basic regressions 
# last modified by casey mcnichols
# last modified on 3.4.25
##########################################

#the purpose of this file is to clean the TRAC data and merge it with similar qcew data

#loading packages
#if we end up scraping
library('rvest')
library('jsonlite')
library('stringr')
library('tidyr')
library('dplyr')
library('zoo')
library('vtable')
library('sf')
library(plm)
library(zoo)

# setting working directory
setwd("C:/Users/casem/Box/undoc/trac")

#loading data
load("matched_employment.Rdata")
load("matched_wages.Rdata")

#creating a yearmon variable and qtr_mon variable
matched_employment$year_mon <- as.yearmon(paste(matched_employment$year, " ", matched_employment$month), "%Y %m")
matched_wages$year_qtr <- as.yearqtr(paste(matched_wages$year, " ", matched_wages$qtr), "%Y %q")

#inverse hyperbolic sin transformation ala marc bellemare
matched_employment$ihs_emp = log(matched_employment$monthly_emplvl + ((matched_employment$monthly_emplvl^2 +1)^0.5))
matched_employment$ihs_count = log(matched_employment$count + ((matched_employment$count^2 +1)^0.5))

matched_wages$ihs_ave_ww = log(matched_wages$avg_wkly_wage + ((matched_wages$avg_wkly_wage^2 +1)^0.5))
matched_wages$ihs_total_count = log(matched_wages$total_value + ((matched_wages$total_value^2 +1)^0.5))

#making county_state variable for duplicate county names
matched_employment$county_state <- paste(matched_employment$county, " ,", matched_employment$state)
matched_wages$county_state <- paste(matched_wages$county, " ,", matched_wages$state)

######################################################
# employment
######################################################

#running a basic regression
emp_regression <- lm(ihs_emp ~  ihs_count + factor(county_state) + factor(year_mon), data = matched_employment)
summary(emp_regression)
#doing this as a panel
employment_panel <- pdata.frame(matched_employment, index=c("county_state","year_mon"))
#running a panel model
emp_regression_plm <- plm(ihs_emp ~  ihs_count, data = employment_panel, model = "within", effect = "twoway")
summary(emp_regression_plm)

######################################################
# wages
######################################################
#running a basic regression
wage_regression <- lm(ihs_ave_ww ~  ihs_total_count + factor(county_state) + factor(year_qtr), data = matched_wages)
summary(wage_regression)
#doing this as a panel
wage_panel <- pdata.frame(matched_wages, index=c("county_state","year_qtr"))
#running a panel model
wage_regression_plm <- plm(ihs_ave_ww ~  ihs_total_count, data = wage_panel, model = "within", effect = "twoway")
summary(wage_regression_plm)

###################################################################################################
