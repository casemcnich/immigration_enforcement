##########################################
# the purpose of this is to merge the dewey data with trac
# for now this is trac data
# last modified by casey mcnichols
# last modified on 9.30.26
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
# for data download
library('deweyr')

# setting the working directory -----------------------
setwd("C:/Users/casem/Box/immigration_enforcement")

# load data -----------------

# Download a specific date range
#download_dewey(
#  api_key = "your-api-key",
#  folder_id = "abc123",
#  partition_key_after = "2014-01-01",
#  partition_key_before = "2019-01-01",
#  download_path = 'C:/Users/casem/Box/immigration_enforcement/data'
#)

# dewey test data
dewey <- read.csv("data/weekly-patterns-plus-sample.csv",
                  stringsAsFactors = FALSE
)

# trac employment merged with nhgis data
trac_wages <- load("data/total_wages_restaurant.rds")


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

dewey_restaurants <- dewey_restaurants %>%
  mutate(
    # less than 5 minutes
    dwell_under_5 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=<5\":)\\d+")
    ),
    
    # 5-20 minutes
    dwell_5_20 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=5-20\":)\\d+")
    ),
    
    # 21-60 min
    dwell_21_60 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=21-60\":)\\d+")
    ),
    
    # 61-240 minutes
    dwell_61_240 = as.numeric(
      str_extract(bucketed_dwell_times, "(?<=61-240\":)\\d+")
    ),
    
    # 240 minutes
    dwell_over_240 = as.numeric(
      str_extract(bucketed_dwell_times, '(?<=">240":)\\d+')
    )
  )

#* summary stats -----------------------------
dewey_restaurants <- dewey_restaurants %>%
  mutate(
    
    # total number of visits
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
#save(
 # dewey_restaurants,
 # file = "data/dewey_restaurants.rds"
#)

#* aggregate by county ----------------------------------------
dewey_county <- dewey_restaurants %>%
  group_by(county_fips) %>%
  summarise(
    # number of restaurants in the county
    restaurants = n(),
    
    # total visits in each dwell-time bucket
    dwell_under_5 = sum(dwell_under_5, na.rm = TRUE),
    dwell_5_20 = sum(dwell_5_20, na.rm = TRUE),
    dwell_21_60 = sum(dwell_21_60, na.rm = TRUE),
    dwell_61_240 = sum(dwell_61_240, na.rm = TRUE),
    dwell_over_240 = sum(dwell_over_240, na.rm = TRUE),
    
    # total visits
    dwell_total = sum(dwell_total, na.rm = TRUE),
    
    # total visits lasting more than hour
    dwell_over_60 = sum(dwell_over_60, na.rm = TRUE),
    .groups = "drop"
  )


#* county-level shares -------------------------------
dewey_county <- dewey_county %>%
  mutate(
    # share of visits lasting over hour
    share_over_60 =
      dwell_over_60 / dwell_total,
    
    # share of visits lasting more than four hours
    share_over_240 =
      dwell_over_240 / dwell_total
  )

#* do some more data quality checks 
dewey_restaurants %>%
  summarise(
    rows = n(),
    unique_stores = n_distinct(id_store),
    unique_placekeys = n_distinct(placekey),
    missing_store_id = sum(is.na(id_store)),
    missing_placekey = sum(is.na(placekey)),
    missing_cbg = sum(is.na(poi_cbg))
  )

#* dwell-time shares -------------------------------------------

dewey_restaurants <- dewey_restaurants %>%
  mutate(
    share_under_5 = if_else(
      dwell_total > 0, dwell_under_5 / dwell_total, NA_real_
    ),
    share_5_20 = if_else(
      dwell_total > 0, dwell_5_20 / dwell_total, NA_real_
    ),
    share_21_60 = if_else(
      dwell_total > 0, dwell_21_60 / dwell_total, NA_real_
    ),
    share_61_240 = if_else(
      dwell_total > 0, dwell_61_240 / dwell_total, NA_real_
    ),
    share_over_240 = if_else(
      dwell_total > 0, dwell_over_240 / dwell_total, NA_real_
    )
  )

# data quality checks -----------------------------------------

# missingness
dewey_restaurants %>%
  summarise(
    n_rows = n(),
    n_stores = n_distinct(id_store),
    n_placekeys = n_distinct(placekey),
    n_counties = n_distinct(county_fips),
    missing_dwell = sum(is.na(bucketed_dwell_times)),
    missing_location = sum(is.na(poi_cbg))
  )

# how the total visits are gonna be distributed
summary(dewey_restaurants$dwell_total)

# extreme values
quantile(
  dewey_restaurants$dwell_total,
  probs = c(0, .25, .5, .75, .9, .95, .99, 1),
  na.rm = TRUE
)

#* save county-level data -------------------------------------
# save(dewey_county, file = "data/dewey_county.Rdata")

# load patterns plus linking data -----------------------------
linking <- read.csv("data/patterns-plus-crosswalk-sample.csv",
                     stringsAsFactors = FALSE
)

# load dewey placekey data ------------------------------------
placekey <- read.csv("data/global-places-poi-geometry-sample.csv",
                  stringsAsFactors = FALSE
)

# filter to just US and food service
placekey <- placekey %>%
  filter(iso_country_code == "US") %>%
  filter(top_category == "Restaurants and Other Eating Places"
  )

# merge all the data ------------------------------------------
# merge dewey and linking data on store_id
dewey_restaurants <- left_join(dewey_restaurants, linking, by = c("id_store" = "id_store"))

# merge dewey and placekey on placekey
dewey_restaurants <- left_join(dewey_restaurants, placekey, by = c("placekey" = "placekey"))

# merge with foia + trac data ---------------------------------
