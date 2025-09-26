################################################################
# cleaning and merging trac data with other trac data and qcew
# last modified by casey mcnichols
# last modified on 9.25
################################################################
# the purpose of this file is to clean the TRAC data and merge it with similar qcew data

#loading packages
library('rvest')
library('jsonlite')
library('stringr')
library('tidyr')
library('dplyr')
library('usdata')
library(lubridate)
library(vtable)
library(sf)

# setting working directory
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")

# loading data -----
load("county_arrests_all_2_18.Rdata")
load("county_arrests_nocap.Rdata")
load("county_arrests_cap.Rdata")

#merging the cap and no cap data with the regular trac data
#trac <- merge(trac, county_arrests_nocap, by = c("title", "month"), all.x = T)

#splitting the cap arrest title and getting rid of the space
county_arrests_cap$title <- gsub(" - Yes", "", county_arrests_cap$title)

trac <- merge(trac, county_arrests_cap, by = c("title", "month"), all.x = T)

# just keeping relevant columns 
trac <- subset(trac, select = c("title", "month", "count", "cap_arrests"))

#makign na columns 0
trac$cap_arrests[is.na(trac$cap_arrests)] <- 0

#for now manually making the no_cap column 
trac$count <- as.numeric(trac$count)
trac$cap_arrests <- as.numeric(trac$cap_arrests)

trac$no_cap_arrests <- trac$count - trac$cap_arrests
# cleaning trac - for the purpose of both dataframe merges
##############################################################################
#splitting the month and years in the same way that the qcew was split 

#expanding out trac
start_date <- as.Date("2014-10-01")
end_date <- as.Date("2018-05-01")

trac$month <- paste0(trac$month, "-01")
trac$month <- as.Date(trac$month)

# Create a data frame of all month-year combinations in the range
all_months <- seq.Date(start_date, end_date, by = "month") %>%
  format("%Y-%m") 

# Expand the dataset
trac <- trac %>%
  mutate(month_year = format(month, "%Y-%m")) %>%
  complete(title, month_year = all_months, fill = list(value = NA))

#fill in NA values with 0 
trac$count[is.na(trac$count)] <- 0
trac$cap_arrests[is.na(trac$cap_arrests)] <- 0
trac$no_cap_arrests[is.na(trac$no_cap_arrests)] <- 0

#drop month and percent
trac <- subset(trac, select = -c(month))

#renaming title to county 
colnames(trac)[colnames(trac) == "title"] <- "county"

#changing washington county CA to nevada county CA
trac$county[trac$county == 'Washington County, CA'] <- 'Nevada County, CA'
trac$county[trac$county == 'Humbolt County, CA'] <- 'Humboldt County, CA'

trac <- trac %>%
  separate(county, into = c("county", "state"), sep = ", ")

#cleaning trac counties so that they are theoretically easier to merge
#there is a space at the end of county i need to remove
trac$county <- gsub("city","City", trac$county)
trac$county <- str_trim(trac$county, side = "right")

#fixing specific trac county names 
trac$county[trac$county == 'Aguadilla'] <- 'Aguadilla Municipio'
trac$county[trac$county == 'San Juan'] <- 'San Juan Municipio'
trac$county[trac$county == 'Fond du Lac County'] <- 'Fond Du Lac County'
trac$county[trac$county == 'Anchorage Municipality'] <- 'Anchorage Municipality'

#splitting the month and years in the same way that the qcew was split 
trac <- trac %>%
  separate(month_year, into = c("year", "month"), sep = "-")

#switching these variables to numeric
trac$year <-as.numeric(trac$year)
trac$month <-as.numeric(trac$month)

#remove uppercase letters that are mid word
trac$county <- gsub("(?<=\\B)([A-Z])", "\\L\\1", trac$county, perl = TRUE)
trac$county <- gsub("O'Brien County", "Obrien County", trac$county, perl = TRUE)

#dropping guam

#dropping unknown
trac <- subset(trac, trac$county!='unknown')
trac <- subset(trac, trac$state!='PR')
trac <- subset(trac, trac$state!='GU')
trac <- subset(trac, trac$state!='MP')


#adjusting a couple of the names
trac$county[trac$county == 'Do?a Ana County'] <- 'Dona Ana County'
trac$county[trac$county == 'De Witt County'] <- 'Dewitt County'
trac$county[trac$county == 'Guaynabo'] <- 'Guaynabo Municipio'

save(trac, file = "trac_cap_nocap_merged.Rdata")

###################################################################
###################################################################
#################pairing with employment data #####################
#loading QCEW#

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
