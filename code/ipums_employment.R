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

mean(data$HOURWAGE_CPIU_2010)
# clean ipums data -----
# drop people who are not in the labor force at all
data <- subset(data, HOURWAGE_CPIU_2010 == 99999.99)

#* Foreign-born non-citizen Hispanics  --------------------------------

#foreign born 
foreign_hisp <- subset(data, NATIVITY == 5 )

# hispanic 
foreign_hisp <- subset(foreign_hisp, HISPAN >= 100 & HISPAN <= 900)

# non citizen 
foreign_hisp_noncitizen <- subset(foreign_hisp, CITIZEN == 5)

#* Foreign-born naturalized citizen Hispanics  --------------------------------
foreign_hisp_naturalized <- subset(foreign_hisp, CITIZEN == 4)

#* Native-born Hispanics --------------------------------
# hispanic 
native_hisp <- subset(data, HISPAN >= 100 & HISPAN <= 900)

# native 
foreign_hisp <- subset(native_hisp, NATIVITY == 1 |NATIVITY == 2|NATIVITY == 3|NATIVITY == 4)

#* Native-born non-Hispanic whites --------------------------------

# native born 
native_nonhisp_white <-  subset(data, NATIVITY == 1 |NATIVITY == 2|NATIVITY == 3|NATIVITY == 4)

# non hispanic white
native_nonhisp_white <- subset (native_nonhisp_white, HISPAN == 000)

# white
native_nonhisp_white <- subset (native_nonhisp_white, RACE == 100)


# merge ipums data------




# load trac data 


