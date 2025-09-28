##########################################
# the purpose of this is to clean and merge the other qcew categories 
#and the construction industry in
# last modified by casey mcnichols
# last modified on 9.26.25
##########################################

# the purpose of this file is to clean the TRAC data and merge it with similar qcew data

#loading packages
#if we end up scraping
library('rvest')
library('jsonlite')
library('stringr')
library('tidyr')
library('dplyr')
library('usdata')
library(lubridate)
library(vtable)
library(sf)
library(zoo)

# setting the working directory
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")

# QCEW Functions ----
#* Function to qcew employment data ----
load_employment_qcew <- function (url_chunk){
  #creating an empty list 
  df_list <- list()  # create empty list
  #vector of years 
  years <- c("2014", "2015", "2016", "2017", "2018", "2019")
  
  for (i in years){
    url <- paste0("../data/qcew/", i , url_chunk, ".csv")
    df <- read.csv(url)
    df_long <- df %>%
      select(area_fips, area_title, own_title, year, qtr,month1_emplvl, month2_emplvl, month3_emplvl) %>%
      pivot_longer(cols = c(month1_emplvl, month2_emplvl, month3_emplvl),
                   names_to = "month",
                   values_to = "monthly_emplvl")
    
    df_long <- df_long %>%
      mutate(qtr = as.numeric(qtr))%>%
      mutate(month = case_when(
        month == "month1_emplvl" & qtr == "1" ~ "1",
        month == "month2_emplvl" & qtr == "1" ~ "2",
        month == "month3_emplvl" & qtr == "1" ~ "3",
        month == "month1_emplvl" & qtr == "2" ~ "4",
        month == "month2_emplvl" & qtr == "2" ~ "5",
        month == "month3_emplvl" & qtr == "2" ~ "6",
        month == "month1_emplvl" & qtr == "3" ~ "7",
        month == "month2_emplvl" & qtr == "3" ~ "8",
        month == "month3_emplvl" & qtr == "3" ~ "9",
        month == "month1_emplvl" & qtr == "4" ~ "10",
        month == "month2_emplvl" & qtr == "4" ~ "11",
        month == "month3_emplvl" & qtr == "4" ~ "12"
      ))
    
    df_list[[i]] <- df_long  # save dataframe "temp_df" to a list, "df_list"
  }
  
  total_employment <- as.data.frame(bind_rows(df_list))
  return(total_employment)
}

#* function to clean qcew employment and wage data ----
clean_qcew <- function(qcew_data){
  
  # Standardize Anchorage
  qcew_data <- qcew_data %>%
    filter(area_title != "Anchorage, AK MSA")
  qcew_data$area_title[qcew_data$area_title == 'Anchorage Borough, Alaska'] <- 
    'Anchorage Municipality, Alaska'
  
  # Split area_title into county and state
  qcew_data <- qcew_data %>%
    separate(area_title, into = c("county", "state_full"), sep = ",", extra = "merge")
  
  # Trim whitespace 
  qcew_data$state_full <- str_trim(qcew_data$state_full)
  
  # Match to abbreviations
  qcew_data$state <- state2abbr(qcew_data$state_full)
  
  # Handle MSA rows 
  qcew_data$state <- ifelse(grepl("MSA", qcew_data$state_full),
                                   gsub(" MSA", "", qcew_data$state_full),
                            qcew_data$state)
  
  qcew_data$state[qcew_data$state_full == 'Puerto Rico']      <- 'PR'
  qcew_data$state[qcew_data$state_full == 'Virgin Islands']   <- 'VI'
  qcew_data$state[qcew_data$county == 'District of Columbia'] <- 'DC'
  
  # Drop D.C.duplicate
  qcew_data <- qcew_data[!(qcew_data$county == "District of Columbia" & 
                             qcew_data$area_fips == 11000), ]
  
  # lowercase
  qcew_data$county <- gsub("(?<=\\B)([A-Z])", "\\L\\1", qcew_data$county, perl = TRUE)
  
  # Fix Anchorage name again
  qcew_data$county[qcew_data$county == 'Anchorage Borough'] <- 'Anchorage Municipality'
  

  # Keep only private employment records
  qcew_data <- qcew_data %>%
    filter(own_title == "Private")
  
  return(qcew_data)
}

#* Function to load QCEW Wage data ---
load_wages_qcew <- function (link){
  #creating an empty list 
  df_wage_list <- list()  # create empty list
  #vector of years 
  years <- c("2014", "2015", "2016", "2017", "2018", "2019")
  
  for (i in years){
    url <- paste0("../data/qcew/", i, link ,".csv")
    df <- read.csv(url)
    df_wage_long <- df %>%
      select(area_fips, area_title, own_title, year, qtr, avg_wkly_wage, month1_emplvl, month2_emplvl, month3_emplvl, total_qtrly_wages) %>%
      mutate(qtr = as.numeric(qtr))
    
    df_wage_list[[i]] <- df_wage_long  # save dataframe "temp_df" to a list, "df_list"
  }
  
  total_wages <- bind_rows(df_wage_list) 
  return(total_wages)
}

# Running QCEW functions ------
# Employment ----
#* Restaurants -----
# load function
total_employment_restaurants <- load_employment_qcew(".q1-q4 7225 NAICS 7225 Restaurants")
# Clean function
total_employment_restaurants <- clean_qcew(total_employment_restaurants)
# Saving
save(total_employment_restaurants, file = "../data/total_employment_restaurants.Rdata")

#* Construction -----
# load function
total_employment_const <- load_employment_qcew(".q1-q4 23 Construction")
# Clean function
total_employment_const <- clean_qcew(total_employment_const)
# Saving
save(total_employment_const, file = "../data/total_employment_const.Rdata")

#* Total ----
# load function
total_employment_all <- load_employment_qcew(".q1-q4 10 Total, all industries")
# Clean function
total_employment_all <- clean_qcew(total_employment_all)
# Saving
save(total_employment_all, file = "../data/total_employment_all.Rdata")

# Wages ----
#* Restaurants -----
total_wages_restaurant <- load_wages_qcew(".q1-q4 7225 NAICS 7225 Restaurants")
total_wages_restaurant <- clean_qcew(total_wages_restaurant)
save(total_wages_restaurant, file = "../data/total_wages_restaurant.Rdata")

#* Construction -----
qcew_data <- load_wages_qcew(".q1-q4 23 Construction")
total_wages_const <- clean_qcew(qcew_data)
save(total_wages_const, file = "../data/total_wages_const.Rdata")

#* Total -----
total_wages_all <- load_wages_qcew(".q1-q4 10 Total, all industries")
total_wages_all <- clean_qcew(total_wages_all)
save(total_wages_all, file = "../data/total_wages_all.Rdata")

# Merging employment and wage data ----
# renaming all the wage and employment columns 
names(total_wages_all)[names(total_wages_all) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_all", "month1_emplvl_all","month2_emplvl_all","month3_emplvl_all","total_qtrly_wages_all")
names(total_wages_const)[names(total_wages_const) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_const", "month1_emplvl_const","month2_emplvl_const","month3_emplvl_const","total_qtrly_wages_const")
names(total_wages_restaurant)[names(total_wages_restaurant) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_rest", "month1_emplvl_rest","month2_emplvl_rest","month3_emplvl_rest","total_qtrly_wages_rest")

names(total_employment_all)[names(total_employment_all) == "monthly_emplvl"] <- "monthly_emplvl_all"
names(total_employment_const)[names(total_employment_const) == "monthly_emplvl"] <- "monthly_emplvl_const"
names(total_employment_restaurants)[names(total_employment_restaurants) == "monthly_emplvl"] <- "monthly_emplvl_rest"

# employment merging
all_merged_employ <- right_join(total_employment_all, total_employment_const, by = c("area_fips","county","state_full", "own_title", "year", "month", "qtr"))
all_merged_employ <- right_join(all_merged_employ, total_employment_restaurants, by = c("area_fips","county","state_full", "own_title", "year", "month", "qtr"))

# wages merging
all_merged_wages<- right_join(total_wages_all, total_wages_const, by = c("area_fips","county","state_full", "own_title", "year", "qtr"))
all_merged_wages <- right_join(all_merged_wages, total_wages_restaurant, by =c("area_fips","county","state_full", "own_title", "year", "qtr"))

#switching months to numeric
all_merged_employ$month <- as.numeric(all_merged_employ$month)
all_merged_employ$month <- as.numeric(all_merged_employ$year)
all_merged_wages$month <- as.numeric(all_merged_wages$year)

#making sure the date ranges match
all_merged_employ <- all_merged_employ %>%
  filter(!(year == 2014 & qtr %in% c(1, 2, 3))) %>%
  filter(!(year == 2018 & qtr %in% c(4, 3))) %>%
  filter(!(year == 2019 )) %>%
  filter(!str_detect(county, "Statewide"))

all_merged_wages <- all_merged_wages %>%
  filter(!(year == 2014 & qtr %in% c(1, 2, 3))) %>%
  filter(!(year == 2018 & qtr %in% c(4, 3))) %>%
  filter(!(year == 2019 )) %>%
  filter(!str_detect(county, "Statewide"))

#saving the merged files
save(all_merged_employ, file = "../data/all_merged_employ.Rdata")
save(all_merged_wages, file = "../data/all_merged_wages.Rdata")

# merging with trac ----
#* loading trac data -----
load("../data/trac_cap_nocap_merged.Rdata")

# Create a quarter column based on the year and month
trac_quarters <- trac %>%
  mutate(qtr = ceiling(month / 3))

# Ensure 'count' is numeric (if not already)
trac_quarters$count <- as.numeric(trac_quarters$count)
trac_quarters$no_cap_arrests <- as.numeric(trac_quarters$count)
trac_quarters$cap_arrests <- as.numeric(trac_quarters$count)

# Aggregate by 'year', 'quarter', and 'county_name'
trac_quarters <- trac_quarters %>%
  group_by(year, qtr, county, state) %>%  # Group by year, quarter, and county
  summarise(
    total_value = sum(count, na.rm = TRUE),
    total_cap = sum(cap_arrests, na.rm = TRUE), 
    total_nocap = sum(no_cap_arrests, na.rm = TRUE),#Sum of values, handling NA if any
    .groups = "drop"  # Ungroup the result
  )

#* Merge employment ----
trac_employment <- left_join(all_merged_employ, trac, by = c("county", "state", "year", "month"))

#dropping the two../data/trac_cap_nocap_merged.Rdata unbalanced counties 
trac_employment <- subset(trac_employment, !(county %in% c("Borden County", "Daggett County")))

#help <- subset(trac_employment, is.na(trac_employment$qtr))

#quick summary statistics
sumtable(trac_employment)

#checking if the panel is balanced
panel_balance <- trac_employment %>%
  group_by(county) %>%
  summarise(num_time_periods = n_distinct(year, month))

summary(trac_employment)

#state level summary df 
state_summary <- trac_employment %>%
  group_by(state, year) %>%
  summarise(
    total_employment = sum(monthly_emplvl_all, na.rm = TRUE),
    total_arrests = sum(count, na.rm = TRUE)
  )

#saving 
#save(trac_employment, file = "../data/matched_employment.Rdata")

#* merge wages ----
trac_wages <- right_join(all_merged_wages, trac_quarters, by = c("county", "state", "year", "qtr"))

#dropping the two unbalanced counties 
trac_wages <- subset(trac_wages, !(county %in% c("Borden County", "Daggett County")))

#check for any unmatched counties 
#help <- subset(trac_wages, is.na(trac_wages$qtr))

#checking if the panel is balanced
panel_balance <- trac_wages %>%
  group_by(county) %>%
  summarise(num_time_periods = n_distinct(year, qtr))

summary(trac_wages)

#save(trac_wages, file = "../data/matched_wages.Rdata")

# making the "employment other" category -----

#* Employment ----
# for the sake of clarity I will call all - construction and restaurant as "other" 
trac_employment$monthly_emplvl_other = trac_employment$monthly_emplvl_all - trac_employment$monthly_emplvl_const - trac_employment$monthly_emplvl_rest

# creating a year_mon variable
trac_employment$year_mon <- as.yearmon(paste(trac_employment$year, " ", trac_employment$month), "%Y %m")

#switching  counts to numeric
trac_employment$count <- as.numeric(trac_employment$count)

#making demeaned version, z score version, and inverse hyperbolic sine version
trac_employment = trac_employment %>%
  group_by(area_fips) %>%
  #z score  version
  mutate(z_score_other = scale(monthly_emplvl_other)) %>%
  mutate(z_score_rest = scale(monthly_emplvl_rest)) %>%  
  mutate(z_score_const = scale(monthly_emplvl_const)) %>%
  #demeaned version
  mutate(demeaned_other = monthly_emplvl_other - mean(monthly_emplvl_other)) %>%
  mutate(demeaned_rest = monthly_emplvl_rest - mean(monthly_emplvl_rest)) %>%
  mutate(demeaned_const = monthly_emplvl_const - mean(monthly_emplvl_const))%>%
  #ihs version
  mutate(ihs_emp_other = log(monthly_emplvl_other + ((monthly_emplvl_other^2 +1)^0.5))) %>%
  mutate (ihs_emp_rest = log(monthly_emplvl_rest + ((monthly_emplvl_rest^2 +1)^0.5))) %>%
  mutate( ihs_emp_const = log(monthly_emplvl_const + ((monthly_emplvl_const^2 +1)^0.5))) %>%
  mutate( ihs_count = log(count + ((count^2 +1)^0.5))) %>%
  mutate( ihs_non_cap = log(no_cap_arrests + ((no_cap_arrests^2 +1)^0.5)))

# Wages ----
#I will calculate the same "employment_other" column and then average it over the three months
trac_wages <- trac_wages %>%
  mutate(month1_emplvl_other = month1_emplvl_all - month1_emplvl_rest - month1_emplvl_const) %>%
  mutate(month2_emplvl_other = month2_emplvl_all - month2_emplvl_rest - month2_emplvl_const) %>%
  mutate(month3_emplvl_other = month3_emplvl_all - month3_emplvl_rest - month3_emplvl_const) %>%
  mutate(avg_emplvl_other = rowMeans(select(., month1_emplvl_other, month2_emplvl_other, month3_emplvl_other), na.rm = TRUE)) %>%
  #then i will subtract out the construction and restaurant industries from the total quarterly wage column
  mutate(total_qtrly_wages_other = total_qtrly_wages_all - total_qtrly_wages_rest - total_qtrly_wages_const) %>%
  #finally i will divide the new quarterly wage column by the average employment column
  mutate(avg_wkly_wage_other = (total_qtrly_wages_other/avg_emplvl_other)/13)

# creating a qtr_mon variable
trac_wages$year_qtr <- as.yearqtr(paste(trac_wages$year, " ", trac_wages$qtr), "%Y %q")

#switching  counts to numeric
trac_wages$total_value <- as.numeric(trac_wages$total_value)

#making demeaned version, z score version, and inverse hyperbolic sine version
trac_wages = trac_wages %>%
  group_by(area_fips) %>%
  #z score  version
  mutate(z_score_other = scale(avg_wkly_wage_other)) %>%
  mutate(z_score_rest = scale(avg_wkly_wage_rest)) %>%  
  mutate(z_score_const = scale(avg_wkly_wage_const)) %>%
  #demeaned version
  mutate(demeaned_other = avg_wkly_wage_other - mean(avg_wkly_wage_other)) %>%
  mutate(demeaned_rest = avg_wkly_wage_rest - mean(avg_wkly_wage_rest)) %>%
  mutate(demeaned_const = avg_wkly_wage_const - mean(avg_wkly_wage_const))%>%
  #ihs version
  mutate(ihs_wage_other = log(avg_wkly_wage_other + ((avg_wkly_wage_other^2 +1)^0.5))) %>%
  mutate (ihs_wage_rest = log(avg_wkly_wage_rest + ((avg_wkly_wage_rest^2 +1)^0.5))) %>%
  mutate( ihs_wage_const = log(avg_wkly_wage_const + ((avg_wkly_wage_const^2 +1)^0.5))) %>%
  mutate( ihs_wage_count = log(total_value + ((total_value^2 +1)^0.5))) %>%
  mutate( ihs_no_cap = log(total_nocap + ((total_nocap^2 +1)^0.5)))
