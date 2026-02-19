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

table(data$OCC)

# merge ipums data----------------------------
load("../data/foia_df.Rdata")

# merge 
data <- merge(foia_df, data, by.x = "FIPS", by.y = "COUNTY", all.y = T)

# subset to just food service
data <- subset(data, OCC == 4000 | OCC == 4010|OCC == 4020| OCC == 4030| OCC == 4050|OCC == 4110|OCC == 4120|OCC == 4140|OCC == 4160)

# checking this worked 
table(data$OCC)
              
# make a year_mon variable
data$year_mon <- paste0(data$YEAR, "-", data$MONTH, "-", "01")
data$year_mon <- as.Date(data$year_mon)

# in labor force check 
data <- subset(data, EMPSTAT>0 & EMPSTAT <30)

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


# summary statistics ------
sumtable(data)
sumtable(foreign_hisp)
sumtable(native_hisp)
sumtable(native_nonhisp_white)
sumtable(foreign_hisp_naturalized)
sumtable(foreign_hisp_noncitizen)

# summary graphs: full sample -----

#* write function  

graph_data_over_time <- function(data){

  data_time <- data %>%
    mutate(year_mon = as.Date(year_mon)) %>% 
    group_by(year_month = floor_date(year_mon, "month")) %>%
    summarise(mean_EARNWEEK = mean(EARNWEEK, na.rm = TRUE)) %>% 
    ungroup() 
  
  graph <- ggplot(data_time, aes(x = year_month, y = mean_EARNWEEK)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(title = "Mean weekly wage",
         x = "Date",
         y = "Mean earnings") +
    theme_minimal()

  print (graph)
}

#* full dataset
graph_data_over_time(data)

#* foreign hispanic noncitizen-----
graph_data_over_time(foreign_hisp_noncitizen)

#* foreign hispanic naturalized citizen -----
graph_data_over_time(foreign_hisp_naturalized)

#* Native born hispanics 

graph_data_over_time(native_hisp)

#* Native born non hispanics 
graph_data_over_time(native_nonhisp_white)

# summary graphs by age group -----

#* full dataset
age_data <- subset(data, AGE >=25 & AGE <=49)
graph_data_over_time(age_data)

#* foreign hispanic noncitizen----
age_foreign_hisp_noncitizen <- subset(foreign_hisp_noncitizen, AGE >=25 & AGE <=49)
graph_data_over_time(age_foreign_hisp_noncitizen)

#* foreign hispanic naturalized citizen -----
age_foreign_hisp_naturalized <- subset(foreign_hisp_naturalized, AGE >=25 & AGE <=49)
graph_data_over_time(age_foreign_hisp_naturalized)

#* Native born hispanics 
age_native_hisp <- subset(native_hisp, AGE >=25 & AGE <=49)
graph_data_over_time(age_native_hisp)

#* Native born non hispanics 
age_native_nonhisp_white <- subset(native_nonhisp_white, AGE >=25 & AGE <=49)
graph_data_over_time(age_native_nonhisp_white)

# summary by gender -----

#* full dataset
male_data <- subset(data, SEX ==1)
female_data <- subset(data, SEX ==2)
graph_data_over_time(male_data)
graph_data_over_time(female_data)

#* foreign hispanic noncitizen----
male_foreign_hisp_noncitizen <- subset(foreign_hisp_noncitizen, SEX ==1)
female_foreign_hisp_noncitizen <- subset(foreign_hisp_noncitizen, SEX ==2)
graph_data_over_time(male_foreign_hisp_noncitizen)
graph_data_over_time(female_foreign_hisp_noncitizen)

#* foreign hispanic naturalized citizen -----
male_foreign_hisp_naturalized <- subset(foreign_hisp_naturalized, SEX ==1)
female_foreign_hisp_naturalized <- subset(foreign_hisp_naturalized, SEX ==2)
graph_data_over_time(female_foreign_hisp_naturalized)
graph_data_over_time(male_foreign_hisp_naturalized)

#* Native born hispanics 
male_native_hisp <- subset(native_hisp, SEX ==1)
female_native_hisp <- subset(native_hisp, SEX ==2)
graph_data_over_time(male_native_hisp)
graph_data_over_time(female_native_hisp)

#* Native born non hispanics 
male_native_nonhisp_white <- subset(native_nonhisp_white, SEX ==1)
female_native_nonhisp_white <- subset(native_nonhisp_white, SEX ==2)
graph_data_over_time(male_native_nonhisp_white)
graph_data_over_time(female_native_nonhisp_white)



# event studies ----------------
#* cleaning function

# Calculate the difference in months between 'yearmon' and 'I247_date' as an integer
clean_data_event <- function(data){
  data <- data %>%
    mutate(
      # Calculate whole months difference (handles NAs safely)
      months_since_I247 = if_else(
        !is.na(month_247a_county),
        interval(month_247a_county, year_mon) %/% months(1),
        NA_integer_
      )
    )
  
  #creating a treatment variable for ever treated and always treated
  data <- data %>% mutate(treat = ifelse(!is.na(c(month_247a_county)), 1, 0)) 
  
  # switching count to numeric 
  data$count <- as.numeric(data$count)
  
  return (data)
}

#* wage event study function -----
event_study <- function(data){
  event_subset <- subset(data, month_247a_county > "2014-01-01")
  
  event_subset <- subset(event_subset, months_since_I247 < 20)
  event_subset <- subset(event_subset, months_since_I247 > -20)
  
  event_subset$year_mon <- as.factor(event_subset$year_mon)
  
  event <- feols(
    EARNWEEK ~ i(months_since_I247, treat, ref = -1)+ factor(year_mon) + State | County.x,
    data = event_subset,   cluster = ~ County.x)
  
 print(summary(event))
 
 # graph
 graph <- iplot(event,
       xlab = "Months Since I247",
       ylab = "Effect on Weekly Earnings",
       main = "Event Study: Effect of I247 on Earnings",
       ref.line = 0)
 
 print(graph)
}

#* full dataset -----
# running event study cleaning function
data <- clean_data_event(data)

# run the event study
event_study (data)

#* foreign born non citizen hispanic ----
# running event study cleaning function
foreign_hisp_noncitizen <- clean_data_event(foreign_hisp_noncitizen)
# run the event study
event_study (foreign_hisp_noncitizen)

#*  foreign born naturalized citizen hispanic ----

# running event study cleaning function
foreign_hisp_naturalized <- clean_data_event(foreign_hisp_naturalized)

# run the event study
event_study (foreign_hisp_naturalized)

#* native born hispanic -----
# running event study cleaning function
native_hisp <- clean_data_event(native_hisp)

# run the event study
event_study (native_hisp)

#* native born nonhispanic whites ----
# running event study cleaning function
native_nonhisp_white <- clean_data_event(native_nonhisp_white)

# run the event study
event_study (native_nonhisp_white)








