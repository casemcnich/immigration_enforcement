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
setwd("C:/Users/casem/Box/undoc/trac")

setwd("C:/Users/casem/Box/undoc")


# Loading and cleaning QCEW data ----

#creating an empty list 
df_list <- list()  # create empty list
#vector of years 
years <- c("2014", "2015", "2016", "2017", "2018", "2019")

for (i in years){
  url <- paste0("../qcew/", i ,".q1-q4 7225 NAICS 7225 Restaurants.csv")
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

total_employment_restaurants <- do.call(rbind, df_list) 

save(total_employment_restaurants, file = "total_employment_restaurants.Rdata")

# cleaning qcew 
##############################################################################
#making anchorage constistently a municapility not a boroguh
total_employment_restaurants <- total_employment_restaurants[!(total_employment_restaurants$area_title %in% "Anchorage, AK MSA"),]
total_employment_restaurants$area_title[total_employment_restaurants$area_title == 'Anchorage Borough, Alaska'] <- 'Anchorage Municipality, Alaska'

#getting a unique list of every county 
#split at the comma
total_employment <- total_employment_restaurants %>%
  separate(area_title, into = c("county", "state_full"), sep = ",")
#matching abbreviations to the qcew data
# Create a logical vector where TRUE means there was a valid match
total_employment$state <- state2abbr(total_employment$state_full) #matching the state with the abbreviation
#fixing a few by hand 
total_employment$state <- ifelse(grepl("MSA", total_employment$state_full), total_employment$state_full, total_employment$state)
#getting rid of msa
total_employment$state <- gsub(" MSA","", total_employment$state)

#fixing puerto rico and virgin islands
total_employment$state[total_employment$state_full == ' Puerto Rico'] <- 'PR'
total_employment$state[total_employment$state_full == ' Virgin Islands'] <- 'VI'
total_employment$state[total_employment$county == 'District of Columbia'] <- 'DC'

#dropping 
total_employment <- total_employment[!(total_employment$county %in% "District of Columbia" & total_employment$area_fips == 11000),]

#removing mid-word capitalization
#remove uppercase letters that are mid word
total_employment$county <- gsub("(?<=\\B)([A-Z])", "\\L\\1", total_employment$county, perl = TRUE)

#switching these variables to numeric
total_employment$year <-as.numeric(total_employment$year)
total_employment$month <-as.numeric(total_employment$month)

total_employment <- subset(total_employment, own_title == "Private")
#drop NA
total_employment$county[trac$county == 'Anchorage Borough'] <- 'Anchorage Municipality'

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
################################################################

# break #

###################################################################
####################### pairing with wage data #####################
#creating an empty list 
df_wage_list <- list()  # create empty list
#vector of years 
years <- c("2014", "2015", "2016", "2017", "2018", "2019")

for (i in years){
  url <- paste0("../qcew/", i ,".q1-q4 7225 NAICS 7225 Restaurants.csv")
  df <- read.csv(url)
  df_wage_long <- df %>%
    select(area_fips, area_title, own_title, year, qtr, avg_wkly_wage, month1_emplvl, month2_emplvl, month3_emplvl, total_qtrly_wages) %>%
    mutate(qtr = as.numeric(qtr))
  
  df_wage_list[[i]] <- df_wage_long  # save dataframe "temp_df" to a list, "df_list"
}

total_wages_restaurant <- do.call(rbind, df_wage_list) 

save(total_wages_restaurant, file = "total_wages_restaurant.Rdata")

# cleaning qcew 
##############################################################################

#making anchorage constistently a municapility not a boroguh
total_wages_restaurant <- total_wages_restaurant[!(total_wages_restaurant$area_title %in% "Anchorage, AK MSA"),]
total_wages_restaurant$area_title[total_wages_restaurant$area_title == 'Anchorage Borough, Alaska'] <- 'Anchorage Municipality, Alaska'

#getting a unique list of every county 
#split at the comma
total_wages <- total_wages_restaurant %>%
  separate(area_title, into = c("county", "state_full"), sep = ",")

#matching abbreviations to the qcew data
# Create a logical vector where TRUE means there was a valid match
total_wages$state <- state2abbr(total_wages$state_full) #matching the state with the abbreviation
#fixing a few by hand 
total_wages$state <- ifelse(grepl("MSA", total_wages$state_full), total_wages$state_full, total_wages$state)
#getting rid of msa
total_wages$state <- gsub(" MSA","", total_wages$state)

#fixing puerto rico and virgin islands
total_wages$state[total_wages$state_full == ' Puerto Rico'] <- 'PR'
total_wages$state[total_wages$state_full == ' Virgin Islands'] <- 'VI'
total_wages$state[total_wages$county == 'District of Columbia'] <- 'DC'

#dropping district of columbia MSA duplicate
total_wages <- total_wages[!(total_wages$county %in% "District of Columbia" & total_wages$area_fips == 11000),]

#removing mid-word capitalization
#remove uppercase letters that are mid word
total_wages$county <- gsub("(?<=\\B)([A-Z])", "\\L\\1", total_wages$county, perl = TRUE)

#switching these variables to numeric
total_wages$year <-as.numeric(total_wages$year)
total_wages$month <-as.numeric(total_wages$qtr)

#subsetting to just look at private industry wages
total_wages <- subset(total_wages, own_title == "Private")
#drop NA
total_wages$county[total_wages$county == 'Anchorage Borough'] <- 'Anchorage Municipality'
###############################################################################

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























###################################################################################
############################## employment #########################################

######### total ###############
#creating an empty list 
df_list_all <- list()  # create empty list
#vector of years 
years <- c("2014", "2015", "2016", "2017", "2018", "2019")
#importing industries
for (i in years){
  url <- paste0("../qcew/", i ,".q1-q4 10 Total, all industries.csv")
  df <- read.csv(url)
  df_long <- df %>% #pivoting and dropping unneccessary columns
    select(area_fips, area_title, own_title, year, qtr,month1_emplvl, month2_emplvl, month3_emplvl) %>%
    pivot_longer(cols = c(month1_emplvl, month2_emplvl, month3_emplvl),
                 names_to = "month",
                 values_to = "monthly_emplvl")
  
  df_long <- df_long %>% #turning quarters into months and expanding out our dataframe
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
  
  df_list_all[[i]] <- df_long  # save dataframe "temp_df" to a list, "df_list"
}
#saving file
total_employment_all <- do.call(rbind, df_list_all) 

########### construction ##################
#creating an empty list 
df_list_const <- list()  # create empty list
#vector of years 
years <- c("2014", "2015", "2016", "2017", "2018", "2019")

for (i in years){ #import loop
  url <- paste0("../qcew/", i ,".q1-q4 23 Construction.csv")
  df <- read.csv(url)
  df_long <- df %>%
    select(area_fips, area_title, own_title, year, qtr, avg_wkly_wage, month1_emplvl, month2_emplvl, month3_emplvl, total_qtrly_wages) %>%
    pivot_longer(cols = c(month1_emplvl, month2_emplvl, month3_emplvl),
                 names_to = "month",
                 values_to = "monthly_emplvl")
  
  df_long <- df_long %>%
    mutate(qtr = as.numeric(qtr))%>%
    mutate(month = case_when(#turning quarters into months and expanding out our dataframe
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
  
  df_list_const[[i]] <- df_long  # save dataframe "temp_df" to a list, "df_list"
}
#saving file
total_employment_const <- do.call(rbind, df_list_const) 

################################ wages ############################################
###################################################################################
#creating an empty list 
df_wage_list_all <- list()  # create empty list
#vector of years 
years <- c("2014", "2015", "2016", "2017", "2018", "2019")

for (i in years){
  url <- paste0("../qcew/", i ,".q1-q4 10 Total, all industries.csv")
  df <- read.csv(url)
  df_wage_long <- df %>%
    select(area_fips, area_title, own_title, year, qtr, avg_wkly_wage, month1_emplvl, month2_emplvl, month3_emplvl, total_qtrly_wages) %>%
    mutate(qtr = as.numeric(qtr))
  
  df_wage_list_all[[i]] <- df_wage_long  # save dataframe "temp_df" to a list, "df_list"
}
#saving file
total_wages_all <- do.call(rbind, df_wage_list_all) 

################## construction ########################################

#creating an empty list 
df_wage_list_const <- list()  # create empty list
#vector of years 
years <- c("2014", "2015", "2016", "2017", "2018", "2019")

for (i in years){
  url <- paste0("../qcew/", i ,".q1-q4 23 Construction.csv")
  df <- read.csv(url)
  df_wage_long <- df %>%
    select(area_fips, area_title, own_title, year, qtr, avg_wkly_wage, month1_emplvl, month2_emplvl, month3_emplvl, total_qtrly_wages) %>%
    mutate(qtr = as.numeric(qtr))
  
  df_wage_list_const[[i]] <- df_wage_long  # save dataframe "temp_df" to a list, "df_list"
}
#saving file
total_wages_const <- do.call(rbind, df_wage_list_const) 


#########################################################################
########### binding these together and saving ###########################
#loading the qcew restaurant files
load ("total_wages_restaurant.Rdata")
load ("total_employment_restaurants.Rdata")

#subset to just include private industry
total_employment_all <- subset(total_employment_all, own_title == "Private")
total_employment_const <- subset(total_employment_const, own_title == "Private")
total_employment_restaurants <- subset(total_employment_restaurants, own_title == "Private")
total_wages_all <- subset(total_wages_all, own_title == "Private")
total_wages_const <- subset(total_wages_const, own_title == "Private")
total_wages_restaurant <- subset(total_wages_restaurant, own_title == "Private")

#renaming all the wage and employment columns 
names(total_wages_all)[names(total_wages_all) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_all", "month1_emplvl_all","month2_emplvl_all","month3_emplvl_all","total_qtrly_wages_all")
names(total_wages_const)[names(total_wages_const) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_const", "month1_emplvl_const","month2_emplvl_const","month3_emplvl_const","total_qtrly_wages_const")
names(total_wages_restaurant)[names(total_wages_restaurant) %in% c("avg_wkly_wage", "month1_emplvl","month2_emplvl","month3_emplvl","total_qtrly_wages")] <-c("avg_wkly_wage_rest", "month1_emplvl_rest","month2_emplvl_rest","month3_emplvl_rest","total_qtrly_wages_rest")

names(total_employment_all)[names(total_employment_all) == "monthly_emplvl"] <- "monthly_emplvl_all"
names(total_employment_const)[names(total_employment_const) == "monthly_emplvl"] <- "monthly_emplvl_const"
names(total_employment_restaurants)[names(total_employment_restaurants) == "monthly_emplvl"] <- "monthly_emplvl_rest"

#employment#
other_emp <- right_join(total_employment_all, total_employment_const, by = c("area_fips","area_title","own_title", "year", "month", "qtr"))
all_merged_employ <- right_join(other_emp, total_employment_restaurants, by =c("area_fips","area_title","own_title", "year", "month", "qtr"))
#wages#
other_wages<- right_join(total_wages_all, total_wages_const, by = c("area_fips", "area_title", "own_title", "year", "qtr"))
all_merged_wages <- right_join(other_wages, total_wages_restaurant, by =c("area_fips","area_title","own_title", "year", "qtr"))

#saving the merged files
#NOTE: THESE ARE THE FILES WE USE IN "PROOF OF CONCEPT"
save(all_merged_employ, file = "all_merged_employ.Rdata")
save(all_merged_wages, file = "all_merged_wages.Rdata")

#merging in the trac arrests
load("matched_wages.Rdata")
load("matched_employment.Rdata")

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

#subset
trac_employment<- left_join(all_merged_employ, matched_employment, by = c("area_fips", "own_title", "year", "month", "qtr"))
trac_wages <- left_join(all_merged_wages, matched_wages, by =c("area_fips","own_title", "year", "qtr"))

#total_non_cap 
save(trac_employment, file = "trac_employment.Rdata")
save(trac_wages, file = "trac_wages.Rdata")
