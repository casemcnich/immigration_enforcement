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
library("viridis")
library('vtable')
library('sf')
library('tidyverse')
library('data.table')
library('zipcodeR')
library('tmap')
library('devtools')

# wd 
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")

# download ipums data -----
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("../data/cps_00027.xml")
data <- read_ipums_micro(ddi)

# this is not as relevant because not everyone should be paid hourly
mean(data$HOURWAGE_CPIU_2010)

# clean ipums data and merge with 287 data -----

table(data$COUNTY)
data <- subset(data, COUNTY!= 0)

table(data$OCC)

# foia aata
load("../data/foia_df.Rdata")

# merge 
data <- merge(foia_df, data, by.x = "FIPS", by.y = "COUNTY", all.y = T)

# make a year_mon variable
data$year_mon <- paste0(data$YEAR, "-", data$MONTH, "-", "01")
data$year_mon <- as.Date(data$year_mon)

# in labor force check 
data <- subset(data, EMPSTAT>0 & EMPSTAT <12)

# drop anyone who is not in universe 
data <- subset(data,EARNWEEK < 9999.99)

# make a nativity variable 
data_to_map <- data %>%
  mutate(group = case_when(
    NATIVITY == 5 & HISPAN >= 100 & HISPAN <= 900 & CITIZEN == 5 ~ "foreign_hisp_noncitizen",
    NATIVITY == 5 & HISPAN >= 100 & HISPAN <= 900 & CITIZEN == 4 ~ "foreign_hisp_naturalized",
    NATIVITY %in% c(1,2,3,4) & HISPAN >= 100 & HISPAN <= 900 ~ "native_hisp",
    NATIVITY %in% c(1,2,3,4) & HISPAN == 0 & RACE == 100 ~ "native_nonhisp_white",
    TRUE ~ NA_character_
  ))

# subset to just food service
data <- subset(data_to_map, OCC == 4000 | OCC == 4010|OCC == 4020| OCC == 4030| OCC == 4050|OCC == 4110|OCC == 4120|OCC == 4140|OCC == 4160)

# checking this worked 
table(data$OCC)

foreign_hisp_noncitizen <- subset (data, group == "foreign_hisp_noncitizen")
foreign_hisp_naturalized <- subset(data, group == "foreign_hisp_naturalized")
native_hisp <- subset(data, group == "native_hisp")
native_nonhisp_white <- subset(data, group == "native_nonhisp_white")


# summary statistics ------
sumtable(data)
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
    summarise(mean_EARNWEEK = weighted.mean(EARNWEEK, WTFINL, na.rm = TRUE)) %>% 
    ungroup() 
  
  graph <- ggplot(data_time, aes(x = year_month, y = mean_EARNWEEK)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(title = "Mean weekly wage",
         x = "Date",
         y = "Mean earnings") +
    theme_minimal()

  print (graph)
}

#* full dataset -----
graph_data_over_time(data)
ggsave("../graphs/full_data_overtime.jpeg", width = 10, height = 5)

#* foreign hispanic noncitizen-----
graph_data_over_time(foreign_hisp_noncitizen)
ggsave("../graphs/foreign_hisp_noncitizen_overtime.jpeg", width = 10, height = 5)


#* foreign hispanic naturalized citizen -----
graph_data_over_time(foreign_hisp_naturalized)
ggsave("../graphs/foreign_hisp_naturalized_overtime.jpeg", width = 10, height = 5)

#* Native born hispanics -----

graph_data_over_time(native_hisp)
ggsave("../graphs/native_hisp_overtime.jpeg", width = 10, height = 5)


#* Native born non hispanics ----
graph_data_over_time(native_nonhisp_white)
ggsave("../graphs/native_nonhisp_white.jpeg", width = 10, height = 5)

# weighted combined graph -----

compare_all_groups <- function(data) {
  
  plot_data <- data %>%
    filter(!is.na(group)) %>%
    mutate(year_mon = as.Date(year_mon)) %>%
    group_by(group, year_month = floor_date(year_mon, "month")) %>%
    summarise(
      mean_EARNWEEK = weighted.mean(EARNWEEK, WTFINL, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(plot_data, aes(x = year_month, 
                        y = mean_EARNWEEK, 
                        color = group)) +
    geom_line(linewidth = 0.6) +  
    labs(title = "Mean Weekly Earnings Over Time",
         x = "Date",
         y = "Weighted Mean Weekly Earnings",
         color = "Group") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

compare_all_groups(data)
ggsave("../graphs/allgroups_overtime.jpeg", width = 10, height = 5)


# summary graphs by age group -----

#* full dataset ----
age_data <- subset(data, AGE >=25 & AGE <=49)
graph_data_over_time(age_data)
ggsave("../graphs/data_age.jpeg", width = 10, height = 5)


#* foreign hispanic noncitizen----
age_foreign_hisp_noncitizen <- subset(foreign_hisp_noncitizen, AGE >=25 & AGE <=49)
graph_data_over_time(age_foreign_hisp_noncitizen)
ggsave("../graphs/foreign_hisp_noncitizen_age.jpeg", width = 10, height = 5)

#* foreign hispanic naturalized citizen -----
age_foreign_hisp_naturalized <- subset(foreign_hisp_naturalized, AGE >=25 & AGE <=49)
graph_data_over_time(age_foreign_hisp_naturalized)
ggsave("../graphs/foreign_hisp_naturalized_age.jpeg", width = 10, height = 5)

#* Native born hispanics -----
age_native_hisp <- subset(native_hisp, AGE >=25 & AGE <=49)
graph_data_over_time(age_native_hisp)
ggsave("../graphs/native_hisp_age.jpeg", width = 10, height = 5)

#* Native born non hispanics -----
age_native_nonhisp_white <- subset(native_nonhisp_white, AGE >=25 & AGE <=49)
graph_data_over_time(age_native_nonhisp_white)
ggsave("../graphs/native_nonhisp_white_age.jpeg", width = 10, height = 5)

# summary by gender -----

#* full dataset -----
male_data <- subset(data, SEX ==1)
female_data <- subset(data, SEX ==2)
graph_data_over_time(male_data)
ggsave("../graphs/data_male.jpeg", width = 10, height = 5)
graph_data_over_time(female_data)
ggsave("../graphs/data_female.jpeg", width = 10, height = 5)


#* foreign hispanic noncitizen----
male_foreign_hisp_noncitizen <- subset(foreign_hisp_noncitizen, SEX ==1)
female_foreign_hisp_noncitizen <- subset(foreign_hisp_noncitizen, SEX ==2)
graph_data_over_time(male_foreign_hisp_noncitizen)
ggsave("../graphs/foreign_hisp_noncitizen_male.jpeg", width = 10, height = 5)
graph_data_over_time(female_foreign_hisp_noncitizen)
ggsave("../graphs/foreign_hisp_noncitizen_female.jpeg", width = 10, height = 5)


#* foreign hispanic naturalized citizen -----
male_foreign_hisp_naturalized <- subset(foreign_hisp_naturalized, SEX ==1)
female_foreign_hisp_naturalized <- subset(foreign_hisp_naturalized, SEX ==2)
graph_data_over_time(female_foreign_hisp_naturalized)
ggsave("../graphs/foreign_hisp_naturalitzed_female.jpeg", width = 10, height = 5)
graph_data_over_time(male_foreign_hisp_naturalized)
ggsave("../graphs/foreign_hisp_naturalitzed_male.jpeg", width = 10, height = 5)

#* Native born hispanics -----
male_native_hisp <- subset(native_hisp, SEX ==1)
female_native_hisp <- subset(native_hisp, SEX ==2)
graph_data_over_time(male_native_hisp)
ggsave("../graphs/native_hisp_male.jpeg", width = 10, height = 5)
graph_data_over_time(female_native_hisp)
ggsave("../graphs/native_hisp_female.jpeg", width = 10, height = 5)

#* Native born non hispanics -----
male_native_nonhisp_white <- subset(native_nonhisp_white, SEX ==1)
female_native_nonhisp_white <- subset(native_nonhisp_white, SEX ==2)
graph_data_over_time(male_native_nonhisp_white)
ggsave("../graphs/native_nonhisp_white_male.jpeg", width = 10, height = 5)
graph_data_over_time(female_native_nonhisp_white)
ggsave("../graphs/native_nonhisp_white_female.jpeg", width = 10, height = 5)

# mapping -----
#importing a USA shapefile
usa <- st_read("../data/nhgis0022_shapefile_tl2016_us_county_2016/US_county_2016.shp")

#dropping puerto rico 
usa <- subset(usa, STATEFP != "72")
usa <- subset(usa, STATEFP != "78") #virgin island
usa <- subset(usa, STATEFP != "69") #mariana islands
usa <- subset(usa, STATEFP != "60") #American Samoa
usa <- subset(usa, STATEFP != "15") #Hawaii
usa <- subset(usa, STATEFP != "02") #Alaska
usa <- subset(usa, STATEFP != "66") #guam

# making the map of earnings ----
# aggregating over county/4 years
#* all occupations ------
county_avg <- data_to_map %>%
  group_by(FIPS) %>%
  summarise(
    county_mean_EARNWEEK = weighted.mean(EARNWEEK, WTFINL, na.rm = TRUE),
    .groups = "drop"
  )

earnings_map <- usa %>%
  left_join(county_avg, by = c("GEOID" = "FIPS"))

all <- ggplot(earnings_map) +
  geom_sf(aes(fill = county_mean_EARNWEEK), color = "black", size = 0.1) +
  scale_fill_viridis(option = "viridis", na.value = "white") +
  labs(
    title = "County-Level Mean Weekly Earnings over whole sample",
    fill = "Mean Weekly Earnings"
  ) +
  theme_minimal()
ggsave("../graphs/all_industries.jpeg", width = 10, height = 5)


#* just restaurants -------
county_avg <- data %>%
  group_by(FIPS) %>%
  summarise(
    county_mean_EARNWEEK = weighted.mean(EARNWEEK, WTFINL, na.rm = TRUE),
    .groups = "drop"
  )

# merging back into original data and mapping
earnings_map <- usa %>%
  left_join(county_avg, by = c("GEOID" = "FIPS"))

rest <- ggplot(earnings_map) +
  geom_sf(aes(fill = county_mean_EARNWEEK), color = "black", size = 0.1) +
  scale_fill_viridis(option = "viridis", na.value = "white") +
  labs(
    title = "County-Level Mean Weekly Earnings over whole sample",
    fill = "Mean Weekly Earnings"
  ) +
  theme_minimal()
rest
ggsave("../graphs/restaurants_map.jpeg", width = 10, height = 5)

# event studies ----------------
#* cleaning function ----

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
unique_counties <- data[!duplicated(data$county), ]

# 261 unique counties
table(unique_counties$month_247a_county)


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








