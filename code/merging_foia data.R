######################################
# the purpose of this is to clean and merge the foia data 
# and the construction industry in
# last modified on 10.5.25

# prerequisites ----

#libraries
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
library(stringr)

# wd ----
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")

# loading the three datasets ----
I247a <- read.csv("../data/foia/detainers_10_10_25.csv")
pep <- read.csv("../data/foia/PEP_2015.csv")
df <- read.csv("../data/foia/map_2.csv")

# cleaning detainers ----

# delete any row where the state is missing
I247a <- I247a %>%
  filter(!(nchar(State) == 0 | is.na(State)) & !(nchar(County) == 0 | is.na(County))) %>%
  mutate(I247a_id = row_number())

# checking for duplicate rows (none)
I247a <- I247a %>%
  distinct()

# dropping unnecessary columns 
I247a <- I247a[, !(names(I247a) %in% c(
  "Jurisdiction.Policy.Number..Name.and.Description.of.a.policy.that.limits.cooperation.with.ICE",
  "Additional.Comments",
  "uncooperative", 
  "Month.and.Year.began.accepting.I.247A"                                                        
))]

# drop state prisons
I247a <- subset(I247a, Jail.or.Prison.Type == "County/City")
# drop juvenile prisons
I247a <- subset(I247a, Juvenile.Facility == "NO")

# If not already in Date format, convert the date column
# Convert the date column from day/month/year format to Date class
I247a$month_247a <- as.Date(I247a$month_247a, format = "%d/%m/%Y")

# function to clean county names
clean_county_state <- function(df) {
  # Convert to lowercase first
  df$County <- tolower(df$County)
  # Capitalize first letter of each county name
  substr(df$County, 1, 1) <- toupper(substr(df$County, 1, 1))
  # Trim whitespace on the right side
  df$County <- str_trim(df$County, side = "right")
  # st to saint 
  df$County <- gsub("St\\.", "Saint", df$County)
  # Remove all periods
  df$County <- gsub("\\.", "", df$County)
  # Combine county and state into new column "county_state"
  df$county_state <- paste0(df$County, ", ", df$State)
  return(df)
}

# clean county names
I247a <- clean_county_state (I247a)

# merging the large df with the i247a dataframe ----

# cleaning county names
df <- clean_county_state(df)

# 1 Function to find the closest matching county in df for each county in detainers
find_closest_county <- function(county_name, county_list) {
  distances <- stringdist::stringdist(county_name, county_list, method = "jw")  # Jaro-Winkler distance
  closest_match <- county_list[which.min(distances)]  # Find the county with the smallest distance
  return(closest_match)
}

# 2 Apply fuzzy matching to the 'County' column in detainers dataset
I247a$matched_county <- sapply(I247a$county_state, function(x) find_closest_county(x, df$county_state))

# fixing two counties that had typos and not automatically match 
#crowley 
I247a$matched_county[I247a$`I247a_id` == 1405] <- "Tarrant, TX" 

# decataur
I247a$matched_county[I247a$`I247a_id` == 1413] <- "Wise, TX" 

# 3 Merge on both 'State' and the 'matched_county' column
df_I247a <- left_join(I247a, df, by = c("State" = "State", "matched_county" = "county_state"))

# Fix counties that are real but had no match
# Barry MO is a real county that is not in the df data - will add in FIPS
df_I247a$FIPS[df_I247a$`I247a_id` == 669] <- 29009

#* fixing missing counties -----
# Starke, IN
df_I247a$FIPS[df_I247a$`I247a_id` == 1228] <- 18149

# Moore county Tx
df_I247a$FIPS[df_I247a$`I247a_id` == 1554] <- 48341
df_I247a$FIPS[df_I247a$`I247a_id` == 1423] <- 48341

# morris county tx
df_I247a$FIPS[df_I247a$`I247a_id` == 1555] <- 48343

# williams county oh
df_I247a$FIPS[df_I247a$`I247a_id` == 1827] <- 39173

# drop beaver county 
# drop beaver county TX - does not exist?
df_I247a <- df_I247a[df_I247a$county_state != "Beaver, TX", ]

#* resolving duplicate county observations with different first i247a dates
# there are multiple jails in a county - > if any of the jails participate in the detainer program then 
# detainer county ==1
# so in this case a few duplicates are okay and the rows are functionally the same if I select 
# the first item

# there are 4 cases of multiple join 

# drop empty county 
df_I247a <- df_I247a[df_I247a$I247a_id != 3485, ]

# dealing with different I247 by county
df_I247a <- df_I247a %>%
  group_by(FIPS) %>%
  mutate(flag = n_distinct(month_247a) > 1) %>%
  ungroup()

table(df_I247a$flag)

# there are 48 issues in the data representing 10 conflicting counties (most large)
fix <- subset(df_I247a, flag == 1)

# if there are multiple jails in a county with different policies, go off the most 
# pro ice policy since this would impact people the most
# OR go off of the contents of the map file

# making new variables "month_247a_county" which is at the county level 
df_I247a$month_247a_county <- df_I247a$month_247a

# adjusting first treatment date for certain counties 
df_I247a$month_247a_county[df_I247a$FIPS == 53077] <- as.Date("2014-01-01") #Yakima
df_I247a$always_treated[df_I247a$FIPS == 53077] <- 1 #Yakima
df_I247a$never_treated[df_I247a$FIPS == 53077] <- 0 #Yakima

df_I247a$month_247a_county[df_I247a$FIPS == 53061] <- NA #Snohomish
df_I247a$always_treated[df_I247a$FIPS == 53061] <- 0 #Snohomish
df_I247a$never_treated[df_I247a$FIPS == 53061] <- 1 #Snohomish

df_I247a$month_247a_county[df_I247a$FIPS == 6065] <- NA #Riverside
df_I247a$always_treated[df_I247a$FIPS == 6065] <- 0 #Riverside
df_I247a$never_treated[df_I247a$FIPS == 6065] <- 1 #Riverside

df_I247a$month_247a_county[df_I247a$FIPS == 53053] <- as.Date("1999-01-01") #Pierce
df_I247a$always_treated[df_I247a$FIPS == 53053] <- 1 #Pierce
df_I247a$never_treated[df_I247a$FIPS == 53053] <- 0 #Pierce

df_I247a$month_247a_county[df_I247a$FIPS == 35035] <- as.Date("1999-01-01") #Otero
df_I247a$always_treated[df_I247a$FIPS == 35035] <- 1 #Otero
df_I247a$never_treated[df_I247a$FIPS == 35035] <- 0 #Otero

df_I247a$month_247a_county[df_I247a$FIPS == 6059] <- as.Date("1999-01-01") #orange
df_I247a$always_treated[df_I247a$FIPS == 6059] <- 1 #orange
df_I247a$never_treated[df_I247a$FIPS == 6059] <- 0 #orange

df_I247a$month_247a_county[df_I247a$FIPS == 25017] <- as.Date("2017-02-01") #middlsex
df_I247a$always_treated[df_I247a$FIPS == 25017] <- 0 #middlsex
df_I247a$never_treated[df_I247a$FIPS == 25017] <- 0 #middlsex

df_I247a$month_247a_county[df_I247a$FIPS == 12083] <- as.Date("1999-01-01") #marion
df_I247a$always_treated[df_I247a$FIPS == 12083] <- 1 #marion
df_I247a$never_treated[df_I247a$FIPS == 12083] <- 0 #marion

df_I247a$month_247a_county[df_I247a$FIPS == 4013] <- as.Date("2017-02-01") #maricopa
df_I247a$always_treated[df_I247a$FIPS == 4013] <- 1 #marion
df_I247a$never_treated[df_I247a$FIPS == 4013] <- 0 #marion

df_I247a$month_247a_county[df_I247a$FIPS == 6037] <- as.Date("2017-01-01") #la
df_I247a$always_treated[df_I247a$FIPS == 6037] <- 1 #la
df_I247a$never_treated[df_I247a$FIPS == 6037] <- 0 #la

# fixing dummy variables
# if month_247 is < 2014, then always treated is yes
df_I247a$always_treated[df_I247a$month_247a_county < as.Date("2014-01-01")] <- 1
# if always treated, year = 1999
df_I247a$month_247a_county[df_I247a$always_treated == 1] <- as.Date("1999-01-01") #la

#checking this worked 
table(df_I247a$month_247a_county, df_I247a$always_treated)
table(df_I247a$month_247a_county, df_I247a$never_treated)

# pep -----
#delete any row where the state is missing
pep <- pep %>%
  filter(!(nchar(State) == 0 | is.na(State)) & !(nchar(County) == 0 | is.na(County))) %>%
  mutate(pep_id = row_number())

#deleting duplicate rows
pep <- pep %>%
  distinct()

# 1. Function to find the closest matching county in df for each county in detainers
find_closest_county <- function(county_name, county_list) {
  distances <- stringdist::stringdist(county_name, county_list, method = "jw")  # Jaro-Winkler distance
  closest_match <- county_list[which.min(distances)]  # Find the county with the smallest distance
  return(closest_match)
}

# 2. Apply fuzzy matching to the 'County' column in detainers dataset
pep$matched_county <- sapply(pep$County, function(x) find_closest_county(x, df$County))

# 3. Merge on both 'State' and the 'matched_county' column
pep_df <- left_join(pep, df, by = c("State" = "State", "matched_county" = "County"))

# If not already in Date format, convert the date column
pep_df <- pep_df %>%
  mutate(month_no_detainers = as.Date(month_no_detainers))  # Adjust the column name if needed

#dropping 0s
pep_df <- pep_df[pep_df$FIPS != 0, ]
pep_df <- pep_df[!is.na(pep_df$FIPS), ]

# COME BACK TO FIX THESE

#padding out the values with 0s
pep_df$FIPS  <- str_pad(pep_df$FIPS, width = 5, side = "left", pad = "0")

# merging pep_df and df_I247a -----
foia_df <- merge(df_I247a, pep_df, by = "FIPS", all.x = T)

# there are some duplicates because there are some counties with multiple jails
# isolate these duplicates
df_no_duplicates <- df[!(duplicated(df_I247a$FIPS) | duplicated(df_I247a$FIPS, fromLast = TRUE)), ]

df_duplicates <- df_I247a[duplicated(df_I247a$FIPS) | duplicated(df_I247a$FIPS, fromLast = TRUE), ]

df_duplicates <- df_duplicates %>%
  group_by(FIPS) %>%
  mutate(
    different_dates = n_distinct(month_247a) > 1,  # TRUE if more than one unique date
    flag = if_else(different_dates, TRUE, FALSE)
  ) %>%
  ungroup()

table(df_flagged$flag)


######## pairing with the map and nhgis merging variables #############
#merging with the geo_ids from the tigerline maps
load(file ="trac/match_vars.Rdata")

df_I247a <- merge(
  match_vars,
  df_I247a,
  by.x = c("GEOID", "abbrev"),
  by.y = c("FORMATTED.FIPS", "State"),
  all.x = TRUE
)

#subsetting so counties are distinct
df_I247a <- df_I247a %>%
  distinct(GEOID, .keep_all = TRUE)

save(df_I247a, file = "df_I247a.Rdata")

############ merging with nhgis data ######################
load("nhgis.Rdata")

########### only keep relevant rows #######################
nhgis <-subset(nhgis, select = c("GISJOIN", "YEAR" ,"AAA5E001", "AAB5E008"))
#merging - each pep_df will merge to multiple Nhgis files
nhgis_I247a <- merge(nhgis, df_I247a, by.x = "GISJOIN", by.y = "GISJOIN", all.x = T)

#filtering for ones that have matches
nhgis_I247a <- nhgis_I247a %>% filter(!is.na(matched_county))

#expanding out to all months and years
nhgis_I247a <- nhgis_I247a %>%
  # Create a sequence of months for each year
  tidyr::expand(YEAR, month = 1:12) %>%
  # Join with original data frame to repeat the value for each month
  left_join(nhgis_I247a, by = "YEAR")

#making a running year_mon column 
# Assuming nhgis_I247a already has 'YEAR', 'month', and 'I247_date' columns
nhgis_I247a <- nhgis_I247a %>%
  # Create a 'yearmon' column combining YEAR and month
  mutate(yearmon = as.yearmon(paste(YEAR, month, sep = "-"))) %>%
  # Calculate the difference in months between 'yearmon' and 'I247_date' as an integer
  mutate(
    months_since_I247 = as.integer(
      (YEAR - as.numeric(format(I247_date, "%Y"))) * 12 + (month - as.numeric(format(I247_date, "%m")))
    )
  )
#merging with trac column 
load("trac/matched_employment.Rdata")

nhgis_I247a <- merge(nhgis_I247a, matched_employment, by.x = c("GEOID", "YEAR", "month"), by.y = c("area_fips", "year", "month"), all.x = T)
nhgis_I247a$share_foreign_born <- nhgis_I247a$AAB5E008/nhgis_I247a$AAA5E001

#saving this file for mapping
nhgis_I247a <- nhgis_I247a %>% mutate(treat = ifelse(!is.na(I247_date), 1, 0)) 

#subset everything before november of 2014
event_subset <- subset(nhgis_I247a, month_I247a > "Jan 2013"|I247_flag == "FALSE")

##### event study ########
library(fixest)

#creating a treatment variable 
nhgis_I247a <- nhgis_I247a %>% mutate(treat = ifelse(!is.na(I247_date), 1, 0)) 

event_subset <- subset(event_subset, months_since_I247 < 20)
event_subset <- subset(event_subset, months_since_I247 > -20)

                  
event <- feols(
  count ~ i(months_since_I247, treat, ref = -1) + share_foreign_born  + factor(yearmon) + state | county,
  data = event_subset,   cluster = ~county)

summary(event)

class(yearmon)

iplot(event, main = "I247a signatures on arrests", xlab = "Months Since I247a signed", ylab = "Estimate and 95% conf int",   col = "#800020")

# sun and abraham event study staggered treatment
# add in slope

##### merging in wage data ########
load("trac/trac_wages.Rdata")
nhgis_I247a <- merge(nhgis_I247a, trac_employment, by.x = c("GEOID", "YEAR", "month"), by.y = c("area_fips", "year", "month"), all.x = T)


#balance table 
balance <-nhgis_I247a %>%
  group_by(I247_flag) %>%
  summarise(
    foreign_born_mean = mean(share_foreign_born),
    mean_pop = mean(AAA5E001),
    employ_all = mean(monthly_emplvl_all, na.rm = T),
    employ_rest = mean(monthly_emplvl_rest/monthly_emplvl_all,  na.rm = T),
    employ_const = mean(monthly_emplvl_const/monthly_emplvl_all, na.rm = T),
    employ_other = mean(monthly_emplvl_other/monthly_emplvl_all, na.rm = T),
    ICE_access = mean(ICE.Access.to.Jail, na.rm = T), # proportion of TRUE
    hold = mean(Hold.For.ICE, na.rm = T) # proportion of TRUE
  )


nhgis_I247a <- nhgis_I247a %>% mutate(treat = ifelse(!is.na(I247_date), 1, 0)) 

event_subset <- subset(nhgis_I247a, month_I247a > "Jan 2013"|I247_flag == "FALSE")
event_subset <- subset(event_subset, months_since_I247 < 10)
event_subset <- subset(event_subset, months_since_I247 > -10)

#making demeaned version, z score version, and inverse hyperbolic sine version
event_subset = event_subset %>%
  group_by(area_title.x) %>%
  #z score  version
  mutate(z_score_other = scale(monthly_emplvl_other)) %>%
  mutate(z_score_rest = scale(monthly_emplvl_rest)) %>%  
  mutate(z_score_const = scale(monthly_emplvl_const)) %>%
  #demeaned version
  mutate(demeaned_other = monthly_emplvl_other - mean(monthly_emplvl_other)) %>%
  mutate(demeaned_rest = monthly_emplvl_rest - mean(monthly_emplvl_rest)) %>%
  mutate(demeaned_const = monthly_emplvl_const - mean(monthly_emplvl_const)) %>%
   #ihs version
   mutate(ihs_emp_other = log(monthly_emplvl_other + ((monthly_emplvl_other^2 +1)^0.5))) %>%
           mutate (ihs_emp_rest = log(monthly_emplvl_rest + ((monthly_emplvl_rest^2 +1)^0.5))) %>%
           mutate( ihs_emp_const = log(monthly_emplvl_const + ((monthly_emplvl_const^2 +1)^0.5))) %>%

         
event <- feols(
  ihs_emp_rest ~ i(months_since_I247, treat, ref = -1) + share_foreign_born  + factor(yearmon) + abbrev | GEOID,
  data = event_subset,   cluster = ~GEOID)

summary(event)
iplot(event)

event <- feols(
  ihs_emp_const ~ i(months_since_I247, treat, ref = -1) + share_foreign_born  + factor(yearmon) + abbrev | GEOID,
  data = event_subset,   cluster = ~GEOID)

summary(event)
iplot(event)

event <- feols(
  ihs_emp_other ~ i(months_since_I247, treat, ref = -1) + share_foreign_born  + factor(yearmon) + abbrev | GEOID,
  data = event_subset,   cluster = ~GEOID)

summary(event)
iplot(event)

