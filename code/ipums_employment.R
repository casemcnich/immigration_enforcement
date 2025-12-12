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

data <- subset(data, COUNTY!= 0)

# merge ipums data----------------------------
load("../data/foia_df.Rdata")

# merge 
data <- merge(foia_df, data, by.x = "FIPS", by.y = "COUNTY", all.y = T)

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

native_nonhisp_white <- subset(native_nonhisp_white,  NATIVITY == 1 |NATIVITY == 2|NATIVITY == 3|NATIVITY == 4)

# event studies ----------------
# Event study -----
#* cleaning 

# Calculate the difference in months between 'yearmon' and 'I247_date' as an integer
full_df_employ <- full_df_employ %>%
  mutate(
    # Create a date from the YEAR and month columns (assume day = 1)
    current_date = make_date(year, month, 1),
    
    # Calculate whole months difference (handles NAs safely)
    months_since_I247 = if_else(
      !is.na(month_247a_county),
      interval(month_247a_county, current_date) %/% months(1),
      NA_integer_
    )
  )

#creating a treatment variable for ever treated and always treated
full_df_employ <- full_df_employ %>% mutate(treat = ifelse(!is.na(c(month_247a_county)), 1, 0)) 

#* arrests event study -----

event_subset <- subset(full_df_employ, month_247a_county > "2014-01-01")

event_subset <- subset(event_subset, months_since_I247 < 20)
event_subset <- subset(event_subset, months_since_I247 > -20)

event_subset$z_score_cound <- (event_subset$count - mean(event_subset$count)) / sd(event_subset$count)
event_subset$z_score_cons <- (event_subset$monthly_emplvl_const - mean(event_subset$monthly_emplvl_const)) / sd(event_subset$monthly_emplvl_const)

event_subset$current_date_factor <- as.factor(event_subset$current_date)

event <- feols(
  count~ i(months_since_I247, treat, ref = -1)+ factor(current_date_factor) + state | County.x,
  data = event_subset,   cluster = ~ County.x)

summary(event)


#* cleaning --------------- 







