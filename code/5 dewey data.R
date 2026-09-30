##########################################
# the purpose of this is to merge the dewey data with trac
# for now this is trac data
# last modified by casey mcnichols
# last modified on 9.29.26
##########################################
#loading packages
library('jsonlite')
library('stringr')
library('tidyr')
library('dplyr')
library('usdata')
library('lubridate')
library('vtable')
library('sf')
library('zoo')

# setting the working directory -----------------------
setwd("C:/Users/casem/Box/immigration_enforcement")

# load data -----------------
# dewey test data
dewey <- read.csv("data/weekly-patterns-plus-sample.csv",
                  stringsAsFactors = FALSE
)
# trac employment and nhgis data
trac_employment <- load("data/nhgis_qcew_trac_employ.Rdata")


# dewey data cleaning ----------------------
names(dewey) <- names(dewey) %>%
  str_trim()

#* keep restaurant observations -------------------------------
# filter on top categoty
dewey_restaurants <- dewey %>%
  filter(top_category == "Restaurants and Other Eating Places"
  )

#* clean geographic variables ---------------------------------
dewey_restaurants <- dewey_restaurants %>%
  mutate(
    # poi_cbg is the census blokc, first 5 digits are fips
    county_fips = str_sub(poi_cbg, 1, 5),
    # first 2 are state fips
    state_fips = str_sub(poi_cbg, 1, 2)
  )

#* extract dwell-time buckets ---------------------------------
# bucketed_dwell_times variable buckets:
# <5 minutes
# 5-20 minutes
# 21-60 minutes
# 61-240 minutes
# >240 minutes

#* create dwell-time variables --------------------------------
dewey_restaurants <- dewey_restaurants %>%
  mutate(
    # Number of visits lasting less than 5 minutes
    dwell_under_5 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=<5\":)\\d+")
    ),
    
    # Number of visits lasting 5-20 minutes
    dwell_5_20 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=5-20\":)\\d+")
    ),
    
    # Number of visits lasting 21-60 minutes
    dwell_21_60 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=21-60\":)\\d+")
    ),
    
    # Number of visits lasting 61-240 minutes
    dwell_61_240 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=61-240\":)\\d+")
    ),
    
    # Number of visits lasting more than 240 minutes
    dwell_over_240 = as.numeric(
      str_extract(bucketed_dwell_times, "(?=>240\":)\\d+")
    )
  )

#* create summary measures ------------------------------------
dewey_restaurants <- dewey_restaurants %>%
  mutate(
    
    # Total number of visits
    dwell_total =
      dwell_under_5 +
      dwell_5_20 +
      dwell_21_60 +
      dwell_61_240 +
      dwell_over_240,
    
    # Number of visits lasting more than 1 hour
    dwell_over_60 =
      dwell_61_240 +
      dwell_over_240,
    
    # Share of visits lasting more than 1 hour
    share_over_60 =
      dwell_over_60 / dwell_total,
    
    # Share of visits lasting more than 4 hours
    share_over_240 =
      dwell_over_240 / dwell_total
  )
#* save restaurant-level data ---------------------------------

save(
  dewey_restaurants,
  file = "data/dewey_restaurants.Rdata"
)

#* aggregate to county ----------------------------------------
# Add up the dwell-time measures across restaurants
# within each county.
dewey_county <- dewey_restaurants %>%
  group_by(county_fips) %>%
  summarise(
    
    # Number of restaurants in the county
    restaurants = n(),
    
    # Total visits in each dwell-time bucket
    dwell_under_5 = sum(dwell_under_5, na.rm = TRUE),
    dwell_5_20 = sum(dwell_5_20, na.rm = TRUE),
    dwell_21_60 = sum(dwell_21_60, na.rm = TRUE),
    dwell_61_240 = sum(dwell_61_240, na.rm = TRUE),
    dwell_over_240 = sum(dwell_over_240, na.rm = TRUE),
    
    # Total visits
    dwell_total = sum(dwell_total, na.rm = TRUE),
    
    # Total visits lasting more than one hour
    dwell_over_60 = sum(dwell_over_60, na.rm = TRUE),
    
    .groups = "drop"
  )


#* calculate county-level shares -------------------------------
dewey_county <- dewey_county %>%
  mutate(
    # Share of visits lasting more than one hour
    share_over_60 =
      dwell_over_60 / dwell_total,
    
    # Share of visits lasting more than four hours
    share_over_240 =
      dwell_over_240 / dwell_total
  )

#* save county-level data -------------------------------------
save(dewey_county, file = "data/dewey_county.Rdata")