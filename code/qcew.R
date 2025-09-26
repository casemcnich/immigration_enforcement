##########################################
# the purpose of this is to clean and merge the other qcew categories 
#and the construction industry in
# last modified by casey mcnichols
# last modified on 3.11.25
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
  
  total_employment <- do.call(rbind, df_list) 
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
  
  # Convert year/month to numeric
  qcew_data$year  <- as.numeric(qcew_data$year)
  qcew_data$month <- as.numeric(qcew_data$month)
  
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
  
  total_wages <- do.call(rbind, df_wage_list) 
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
save(total_employment_restaurants, file = "total_employment_restaurants.Rdata")

#* Construction -----
# load function
total_employment_const <- load_employment_qcew(".q1-q4 23 Construction")
# Clean function
total_employment_const <- clean_qcew(total_employment_const)
# Saving
save(total_employment_const, file = "total_employment_const.Rdata")

#* Total ----
# load function
total_employment_all <- load_employment_qcew(".q1-q4 10 Total, all industries")
# Clean function
total_employment_all <- clean_qcew(total_employment_all)
# Saving
save(total_employment_all, file = "total_employment_all.Rdata")

# Wages ----
#* Restaurants -----
total_wages_restaurant <- load_wages_qcew(".q1-q4 7225 NAICS 7225 Restaurants")
total_wages_restaurant <- clean_qcew(total_wages_restaurant)
save(total_wages_restaurant, file = "total_wages_restaurant.Rdata")

#* Construction -----
total_wages_const <- load_wages_qcew(".q1-q4 23 Construction")
total_wages_const <- clean_qcew(total_wages_const)
save(total_wages_const, file = "total_wages_const.Rdata")

#* Total -----
total_wages_all <- load_wages_qcew(".q1-q4 10 Total, all industries")
total_wages_all <- clean_qcew(total_wages_all)
save(total_wages_all, file = "total_wages_all.Rdata")

# Merging employment and wage data ----
# renaming all the wage and employment columns 
names(total_wages_all)[names(total_wages_all) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_all", "month1_emplvl_all","month2_emplvl_all","month3_emplvl_all","total_qtrly_wages_all")
names(total_wages_const)[names(total_wages_const) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_const", "month1_emplvl_const","month2_emplvl_const","month3_emplvl_const","total_qtrly_wages_const")
names(total_wages_restaurant)[names(total_wages_restaurant) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_rest", "month1_emplvl_rest","month2_emplvl_rest","month3_emplvl_rest","total_qtrly_wages_rest")

names(total_employment_all)[names(total_employment_all) == "monthly_emplvl"] <- "monthly_emplvl_all"
names(total_employment_const)[names(total_employment_const) == "monthly_emplvl"] <- "monthly_emplvl_const"
names(total_employment_restaurants)[names(total_employment_restaurants) == "monthly_emplvl"] <- "monthly_emplvl_rest"

# employment
other_emp <- right_join(total_employment_all, total_employment_const, by = c("area_fips","area_title","own_title", "year", "month", "qtr"))
all_merged_employ <- right_join(other_emp, total_employment_restaurants, by =c("area_fips","area_title","own_title", "year", "month", "qtr"))

# wages
other_wages<- right_join(total_wages_all, total_wages_const, by = c("area_fips", "area_title", "own_title", "year", "qtr"))
all_merged_wages <- right_join(other_wages, total_wages_restaurant, by =c("area_fips","area_title","own_title", "year", "qtr"))

#switching months to numeric
all_merged_employ$month <- as.numeric(all_merged_employ$month)

#making sure the date ranges match
all_merged_employ <- all_merged_employ %>%
  filter(!(year == 2014 & qtr %in% c(1, 2, 3))) %>%
  filter(!(year == 2018 & qtr %in% c(4, 3))) %>%
  filter(!(year == 2019 )) %>%
  filter(!str_detect(area_title, "Statewide"))


all_merged_wages <- all_merged_wages %>%
  filter(!(year == 2014 & qtr %in% c(1, 2, 3))) %>%
  filter(!(year == 2018 & qtr %in% c(4, 3))) %>%
  filter(!(year == 2019 )) %>%
  filter(!str_detect(area_title, "Statewide"))


#saving the merged files
save(all_merged_employ, file = "all_merged_employ.Rdata")
save(all_merged_wages, file = "all_merged_wages.Rdata")

# merging with trac data -----
load("matched_wages.Rdata")
load("matched_employment.Rdata")

#subset
trac_employment<- left_join(all_merged_employ, matched_employment, by = c("area_fips", "own_title", "year", "month", "qtr"))
trac_wages <- left_join(all_merged_wages, matched_wages, by =c("area_fips","own_title", "year", "qtr"))

#total_non_cap 
save(trac_employment, file = "trac_employment.Rdata")
save(trac_wages, file = "trac_wages.Rdata")

###############################################################################
#merging our two simple dataframes
#merging to our shapefile data
matched_employment <- right_join(total_employment, trac, by = c("county", "state", "year", "month"))
#pulling out the ones to fix by hand
help <- subset(matched_employment, is.na(matched_employment$qtr))
#borden county does not exist in all QCEW years
# Issaquena County does not exist in all qcew years
matched_employment<- matched_employment[!is.na(matched_employment$monthly_emplvl), ]
###################################################################
#quick summary statistics
sumtable(matched_employment)

#checking if the panel is balanced
panel_balance <- matched_employment %>%
  group_by(county) %>%
  summarise(num_time_periods = n_distinct(year, month))

summary(panel_balance)

#dropping the two unbalanced counties 
matched_employment <- subset(matched_employment, !(county %in% c("Borden County", "Daggett County")))

#state level summary df 
state_summary <- matched_employment %>%
  group_by(state, year) %>%
  summarise(
    total_employment = sum(monthly_emplvl, na.rm = TRUE),
    total_arrests = sum(count, na.rm = TRUE)
  )

#saving 
save(matched_employment, file = "matched_employment.Rdata")



# Create a quarter column based on the year and month
trac_wages <- trac %>%
  mutate(qtr = ceiling(month / 3))

# Ensure 'count' is numeric (if not already)
trac_wages$count <- as.numeric(trac_wages$count)
trac_wages$non_cap <- as.numeric(trac_wages$count)
trac_wages$count <- as.numeric(trac_wages$count)


# Aggregate by 'year', 'quarter', and 'county_name'
trac_wages <- trac_wages %>%
  group_by(year, qtr, county, state) %>%  # Group by year, quarter, and county
  summarise(
    total_value = sum(count, na.rm = TRUE),
    total_cap = sum(cap_arrests, na.rm = TRUE), 
    total_nocap = sum(no_cap_arrests, na.rm = TRUE),#Sum of values, handling NA if any
    .groups = "drop"  # Ungroup the result
  )

###############################################################################
#merging our two simple dataframes
#merging to our shapefile data
matched_wages <- right_join(total_wages, trac_wages, by = c("county", "state", "year", "qtr"))

#checking if the panel is balanced
panel_balance <- matched_wages %>%
  group_by(county) %>%
  summarise(num_time_periods = n_distinct(year, qtr))

summary(panel_balance)

#dropping the two unbalanced counties 
matched_wages <- subset(matched_wages, !(county %in% c("Borden County", "Daggett County")))
help <- subset(matched_wages, is.na(matched_wages$qtr))

save(matched_wages, file = "matched_wages.Rdata")

