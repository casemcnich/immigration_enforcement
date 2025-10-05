##########################################
# the purpose of this is to clean and merge the qcew 
# and nhgis data with the scraped trac data.
# This creates our basline data (exept for the FOIA data)
# that we use in the remaineder of the work
# last modified by casey mcnichols
# last modified on 10.01.25
##########################################

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

#* Construction -----
# load function
total_employment_const <- load_employment_qcew(".q1-q4 23 Construction")
# Clean function
total_employment_const <- clean_qcew(total_employment_const)
# Saving

#* Total ----
# load function
total_employment_all <- load_employment_qcew(".q1-q4 10 Total, all industries")
# Clean function
total_employment_all <- clean_qcew(total_employment_all)

# Wages ----
#* Restaurants -----
total_wages_restaurant <- load_wages_qcew(".q1-q4 7225 NAICS 7225 Restaurants")
total_wages_restaurant <- clean_qcew(total_wages_restaurant)

#* Construction -----
qcew_data <- load_wages_qcew(".q1-q4 23 Construction")
total_wages_const <- clean_qcew(qcew_data)

#* Total -----
total_wages_all <- load_wages_qcew(".q1-q4 10 Total, all industries")
total_wages_all <- clean_qcew(total_wages_all)

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
all_merged_employ$year <- as.numeric(all_merged_employ$year)
all_merged_wages$qtr <- as.numeric(all_merged_wages$qtr)
all_merged_wages$year <- as.numeric(all_merged_wages$year)

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

# checking both dataframes are square
employ_square <- all_merged_employ %>%
  group_by(county, state_full) %>%
  summarise(Count = n()) # there are about 40 counties thaat are not

all_merged_employ <- all_merged_employ %>% 
  group_by(county, state_full) %>% 
  filter(n() >= 45)

wage_square <- all_merged_wages %>%
  group_by(county, state_full) %>%
  summarise(Count = n()) # there are about 40 counties that are not
# some are just "unknown" or other catch

all_merged_wages <- all_merged_wages %>% 
  group_by(state_full, county) %>% 
  filter(n() >= 15)

#saving the merged files
save(all_merged_employ, file = "../data/all_merged_employ.Rdata") #163332
save(all_merged_wages, file = "../data/all_merged_wages.Rdata") #54444

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

# checking both dataframes are square
trac_square <- trac %>%
  group_by(county, state) %>%
  summarise(Count = n()) #yes 1966 unique

trac_square <- trac_quarters %>%
  group_by(county, state) %>%
  summarise(Count = n()) #yes 1966 unique

# trac quarter 29490
# trac 86504

#* Merge employment ----
trac_employment <- left_join(all_merged_employ, trac, by = c("county", "state", "year", "month"))
#trac employ 162540 

#fill the trac arrest columns with 0 where they are NA
# Replace NA with 0 in 'col1' and 'col3'
trac_employment$count[is.na(trac_employment$count)] <- 0
trac_employment$cap_arrests[is.na(trac_employment$cap_arrests)] <- 0
trac_employment$no_cap_arrests[is.na(trac_employment$no_cap_arrests)] <- 0

#dropping the two../data/trac_cap_nocap_merged.Rdata unbalanced counties 
trac_employment <- subset(trac_employment, !(county %in% c("Borden County", "Daggett County")))

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

#* merge wages ----
trac_wages <- left_join(all_merged_wages, trac_quarters, by = c("county", "state", "year", "qtr"))
# n = 54189

#dropping the two unbalanced counties 
trac_wages <- subset(trac_wages, !(county %in% c("Borden County", "Daggett County")))

#check for any unmatched counties 
#help <- subset(trac_wages, is.na(trac_wages$qtr))

#checking if the panel is balanced
panel_balance <- trac_wages %>%
  group_by(county) %>%
  summarise(num_time_periods = n_distinct(year, qtr))

summary(trac_wages)

# making the "employment other" category -----

#* Employment ----
# for the sake of clarity I will call all - construction and restaurant as "other" 
trac_employment$monthly_emplvl_other = trac_employment$monthly_emplvl_all - trac_employment$monthly_emplvl_const - trac_employment$monthly_emplvl_rest

# creating a year_mon variable
trac_employment$year_mon <- as.yearmon(paste(trac_employment$year, " ", trac_employment$month), "%Y %m")

#switching  counts to numeric
trac_employment$count <- as.numeric(trac_employment$count)

# Wages ----
#I will calculate the same "employment_other" column and then average it over the three months

#drop staste y 
trac_wages <- subset(trac_wages, select = c(-state.y, -state.x))

#switching various rows to numeric
trac_wages[, 6:21] <- lapply(trac_wages[, 6:21], as.numeric)

trac_wages <- trac_wages %>%
  ungroup %>%
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

# NHGIS ----
#* loading nhgis data ----
df_list <- list()
years <- c("2014", "2015", "2016", "2017", "2018")

for (i in years){ #import loop
  url <- paste0("../data/nhgis/nhgis0023_", i ,"_county.csv")
  df <- read.csv(url)
  df_list[[i]] <- df  # save dataframe "temp_df" to a list, "df_list"
}
# creating dataframe 
nhgis <- do.call(rbind, df_list) 

#* cleaning ----
# Get the list of counties that appear for each year
county_list <- nhgis %>%
  group_by(COUNTY) %>%
  filter(n_distinct(YEAR) == length(unique(nhgis$YEAR))) %>%
  pull(COUNTY) %>%
  unique()

# switch colnames to lowercase
colnames(nhgis) <- tolower(colnames(nhgis))

# Filter the data frame to keep only those counties
nhgis <- nhgis %>% filter(county %in% county_list) %>%
# and to drop variables
  select(-regiona, -divisiona, -countya, -cousuba, -placea, -aianhha,
             anrca, -cbsaa, -csaa, -metdiva, -nectaa, -cnectaa, -nectadiva, -uaa, 
             -cdcurra, -sdelma, -sdseca, -sdunia, -pci, -pumaa, -name_e, 
         -anrca, -name_m, -state)

# modifing the GEOD so it can be easily merged with shorter FIPS
nhgis$area_fips <- gsub("05000US", "", nhgis$geoid, fixed = TRUE)

#switching year to numeric 
nhgis$year <- as.numeric(nhgis$year)

#* merging with other qcew and trac data ----
# wages
trac_wages <- merge(x = trac_wages, y = nhgis, 
                 by.x = c("area_fips", "year"), 
                 by.y = c("area_fips", "year"), 
                 all.x = TRUE)

names(trac_wages)[names(trac_wages) == "county.x"] <- "county"

save(trac_wages, file = "../data/nhgis_qcew_trac_wages.Rdata")

# employment
trac_employment <- merge(x = trac_employment, y = nhgis, 
                    by.x = c("area_fips", "year"), 
                    by.y = c("area_fips", "year"), 
                    all.x = TRUE)

names(trac_employment)[names(trac_employment) == "county.x"] <- "county"

save(trac_employment, file = "../data/nhgis_qcew_trac_employ.Rdata")
