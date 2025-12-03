################################################################
# cleaning and merging trac data with other trac data and qcew
# last modified by casey mcnichols
# last modified on 9.25
################################################################
# the purpose of this file is to clean the TRAC data 

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

# loading TRAC data -----
load("../data/county_arrests.Rdata")
load("../data/county_arrests_nocap.Rdata")
load("../data/county_arrests_cap.Rdata")

# cleaning trac data so that it will merge with itself ----

# removing the cap label from the title 
county_arrests_cap$title <- gsub(" - Yes", "", county_arrests_cap$title)
county_arrests_nocap$title <- gsub(" - No", "", county_arrests_nocap$title)

# merging the data 
trac <- merge(county_arrests_all_2_18, county_arrests_nocap, by = c("title", "month"), all.x = T)
trac <- merge(trac, county_arrests_cap, by = c("title", "month"), all.x = T)

#making na columns 0
trac$cap_arrests[is.na(trac$cap_arrests)] <- 0
trac$cap_arrests[is.na(trac$no_cap_arrestz)] <- 0

# converting to numeric 
trac$count <- as.numeric(trac$count)
trac$cap_arrests <- as.numeric(trac$cap_arrests)
trac$no_cap_arrests <- as.numeric(trac$no_cap_arrests)

#confirming that these sum correctly 
trac$no_cap_arrests_check <- trac$count - trac$cap_arrests

# just keeping relevant columns 
trac <- subset(trac, select = c("title", "month", "count", "cap_arrests", "no_cap_arrests"))

# cleaning for merge with QCEW data -----
#* cleaning dates ----

# Define date range
start_date <- as.Date("2014-10-01")
end_date <- as.Date("2018-05-01")

# Standardize trac$month to Date format
trac$month <- as.Date(paste0(trac$month, "-01"))

# Generate full list 
all_months <- seq.Date(start_date, end_date, by = "month")
all_months_fmt <- format(all_months, "%Y-%m")

# Add month_year column to trac for joining
trac <- trac %>%
  mutate(month_year = format(month, "%Y-%m"))

# Expand dataset
trac <- trac %>%
  complete(title, month_year = all_months_fmt, fill = list(
    count = 0,
    cap_arrests = 0,
    no_cap_arrests = 0
  ))

# drop month
trac <- subset(trac, select = -c(month))

#splitting the month and years in the same way that the qcew was split 
trac <- trac %>%
  separate(month_year, into = c("year", "month"), sep = "-")

#switching these variables to numeric
trac$year <-as.numeric(trac$year)
trac$month <-as.numeric(trac$month)

#* standardizing county names ----
#(in a function so this can be used on other files)
standardize_counties <- 
  function(trac, title){
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
    
    #remove uppercase letters that are mid word
    trac$county <- gsub("(?<=\\B)([A-Z])", "\\L\\1", trac$county, perl = TRUE)
    trac$county <- gsub("O'Brien County", "Obrien County", trac$county, perl = TRUE)
    
    #dropping guam
    trac <- subset(trac, trac$county!='unknown')
    trac <- subset(trac, trac$state!='PR')
    trac <- subset(trac, trac$state!='GU')
    trac <- subset(trac, trac$state!='MP')
    
    
    #adjusting a couple of the names
    trac$county[trac$county == 'Do?a Ana County'] <- 'Dona Ana County'
    trac$county[trac$county == 'De Witt County'] <- 'Dewitt County'
    trac$county[trac$county == 'Guaynabo'] <- 'Guaynabo Municipio'
    
    return(trac)
  }
trac <- standardize_counties(trac, title)

save(trac, file = "../data/trac_cap_nocap_merged.Rdata")

