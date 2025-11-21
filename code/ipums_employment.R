######################################
# the purpose of this is to load and merge ipums employment data

# prerequisites ----
#libraries
library('lubridate')
library('vtable')
library('sf')
library('tidyverse')
library('data.table')
library('zipcodeR')
library('tmap')
library('devtools')
library('dplyr')
library('knitr')
library('zoo')
library('stargazer')
library('stringr')
library('fixest')
library('did')
library("blscrapeR")

# wd ----
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")


# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("../data/cps_00027.xml")
data <- read_ipums_micro(ddi)

# clean ipums data -----
# merge ipums data------

