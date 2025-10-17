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
library('stringr')
library('fixest')

# wd ----
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")

# loading the three datasets ----
I247a <- read.csv("../data/foia/detainers_10_10_25.csv")
pep <- read.csv("../data/foia/PEP_2015.csv")
df <- read.csv("../data/foia/map_2.csv")

# cleaning detainers 2017 data ----

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

# Function to find the closest matching county in df for each county in detainers
find_closest_county <- function(county_name, county_list) {
  distances <- stringdist::stringdist(county_name, county_list, method = "jw")  # Jaro-Winkler distance
  closest_match <- county_list[which.min(distances)]  # Find the county with the smallest distance
  return(closest_match)
}

# Apply fuzzy matching to the 'County' column in detainers dataset
I247a$matched_county <- sapply(I247a$county_state, function(x) find_closest_county(x, df$county_state))

# fixing two counties that had typos and not automatically match 
I247a$matched_county[I247a$`I247a_id` == 1405] <- "Tarrant, TX" # crowley 

I247a$matched_county[I247a$`I247a_id` == 1413] <- "Wise, TX" # decataur

# Merge on both 'State' and the 'matched_county' column
df_I247a <- left_join(I247a, df, by = c("State" = "State", "matched_county" = "county_state"))

# Fix counties that are real but had no match
# Barry MO is a real county that is not in the df data - will add in FIPS
df_I247a$FIPS[df_I247a$`I247a_id` == 669] <- 29009

#* fixing missing counties -----
# Starke, IN
df_I247a$FIPS[df_I247a$`I247a_id` == 1228] <- 18149 # Starke, IN

df_I247a$FIPS[df_I247a$`I247a_id` == 1554] <- 48341 # Moore county Tx

df_I247a$FIPS[df_I247a$`I247a_id` == 1423] <- 48341

df_I247a$FIPS[df_I247a$`I247a_id` == 1555] <- 48343 # morris county tx

df_I247a$FIPS[df_I247a$`I247a_id` == 1827] <- 39173 # williams county oh

df_I247a <- df_I247a[df_I247a$county_state != "Beaver, TX", ] # drop beaver county - does not exist?

#* resolving duplicate county observations with different first i247a dates ----

# RULES: 1) Drop all juvenile or federal prisons (Above)
# 2 - if there are multiple jails in a county - > 
#if any of the jails participate in the detainer program then that counts as treated

# drop empty county 
df_I247a <- df_I247a[df_I247a$I247a_id != 3485, ]

df_I247a <- df_I247a %>%
  group_by(FIPS) %>%
  mutate(
    # Flag always treated
    always_treated = as.integer(any(always_treated == 1)),
    
    # Compute minimum date (even if NA — don't skip)
    min_month = suppressWarnings(min(month_247a, na.rm = TRUE)),  # handle all NA gracefully
    
    # Assign month_247a_county depending on always_treated
    month_247a_county = ifelse(always_treated == 0, min_month, NA),
    
    # Assign never_treated flag if no valid treatment date and not always treated
    never_treated = ifelse(always_treated == 0 & is.na(min_month), 1, 0)
  ) %>%
  ungroup() 

# fixing dummy variables
# if month_247 is < 2014, then always treated is yes
df_I247a$always_treated[df_I247a$month_247a_county < as.Date("2014-01-01")] <- 1
# if always treated, year = 1999
df_I247a$month_247a_county[df_I247a$always_treated == 1] <- as.Date("1999-01-01") #assign arbitrary date to always treate

#padding out fips
df_I247a$FIPS  <- str_pad(df_I247a$FIPS, width = 5, side = "left", pad = "0")


# pep 2015 -----
#delete any row where the state is missing
pep <- pep %>%
  filter(!(nchar(State) == 0 | is.na(State))) %>%
  mutate(pep_id = row_number()) %>%
  #deleting duplicate rows
  distinct()

pep$month_no_detainers <- as.Date(pep$month_no_detainers, format = "%d/%m/%Y")

# delete any full state variables or the weird b6 counties
pep <- pep[ 
  !grepl("7|state of|State of|All counties", pep$County, ignore.case = TRUE), 
]

#* state level variable ----
# the entire states of AL, CA, VM, CT, MA, ME, NH, RH, VA, VM accepted ICE 
# cleaning county and state names with the function we made above
pep <- clean_county_state(pep)

# Apply fuzzy matching to the 'County' column in detainers dataset
pep$matched_county <- sapply(pep$County, function(x) find_closest_county(x, df$County))

# Merge on both 'State' and the 'matched_county' column
pep_df <- left_join(pep, df, by = c("State" = "State", "matched_county" = "County"))

# If not already in Date format, convert the date column
pep_df <- pep_df %>%
  mutate(month_no_detainers = as.Date(month_no_detainers))  # Adjust the column name if needed

pep_df <- pep_df[!(pep_df$pep_id %in% c(5, 2098, 173, 1870, 1513, 1514, 2097,2597,2599,1658, 1681,1682,2101)), ]

# checking merge 
test <- pep_df[is.na(pep_df$FIPS), ]

# fixing legit counties
# fairfield is legit
pep_df$FIPS[pep_df$pep_id == 303|pep_df$pep_id == 304|pep_df$pep_id == 305|pep_df$pep_id == 306 ] <- 09001
# new londan is legit
pep_df$FIPS[pep_df$pep_id == 309|pep_df$pep_id == 310| pep_df$pep_id ==311] <- 09011
# hartford is legit
pep_df$FIPS[pep_df$pep_id == 307| pep_df$pep_id == 308] <- 09003
# butte is legit
pep_df$FIPS[pep_df$pep_id == 603] <- 16023
# hamilton is legit
pep_df$FIPS[pep_df$pep_id == 658] <- 17065

#padding out the values with 0s
pep_df$FIPS  <- str_pad(pep_df$FIPS, width = 5, side = "left", pad = "0")

# merging pep_df and df_I247a -----
foia_df <- merge(df_I247a, pep_df, by = c("FIPS", "State"), all = T)


#* dealing with duplicates by county
foia_df <- foia_df %>%
  group_by(FIPS) %>%
  mutate(dif_detainers = n_distinct(month_no_detainers) > 1) %>%
  ungroup()

# there are 39 duplicate counties in my data that I will need to check by hand
fix <- subset(foia_df, dif_detainers == 1)

# fixing necessary counties - do this better later???
foia_df <- foia_df[!(foia_df$pep_id == 1747 & foia_df$I247a_id == 10), ]
foia_df <- foia_df[!(foia_df$pep_id == 309 & foia_df$pep_id == 310), ] # this one will be dropped anyways
foia_df <- foia_df[!(foia_df$I247a_id == 428 & foia_df$pep_id == 1098), ] # this one will be dropped anyways
foia_df <- foia_df[!(foia_df$pep_id == 1113), ] # this one will be dropped anyways
foia_df <- foia_df[!(foia_df$pep_id == 1821 | foia_df$pep_id == 1822 |foia_df$pep_id == 1823| foia_df$pep_id == 1824), ] # this one will be dropped anyways

#* writing flags of issues ----
foia_df$flag <- ifelse((!is.na(foia_df$month_no_detainers) & foia_df$always_treated == 1), 1, 0)
table(foia_df$flag)
test <- subset(foia_df, flag ==1)
# there is a single observation where the treatment turned off in 2015 that I would need to deal with 
# FIPS = 56037
foia_df$flag2 <- ifelse((foia_df$month_no_detainers > "2014-01-01" & foia_df$never_treated == 1), 1, 0)
table(foia_df$flag2)
test <- subset(foia_df, flag2 ==1)

table(foia_df$Current.Detainer.Notification.Acceptance.Status)

# making a variable for whether or not they currently accept detainers 
foia_df$detainers_2015 <- ifelse(
  foia_df$Current.Detainer.Notification.Acceptance.Status == "4: Willing to accept both (I-247N) Notifications and (I-247D) Detainers" |
    foia_df$Current.Detainer.Notification.Acceptance.Status == "5: Willing to accept (I-247D) Detainers but not (I-247N) Notifications",
  1,
  0
)

# 13 problematic counties - NEED TO CHECK THESE LATER
table(foia_df$detainers_2015, foia_df$never_treated)
fix <- subset(foia_df, detainers_2015 == 1 & never_treated ==1 | flag ==1 | flag2 ==1)
# as we established before, there is a group of folks who switched in 2014, therefore our cutoff should be 2015 likely? or these groups dropped
table(foia_df$detainers_2015, foia_df$always_treated)

# keep first observation 
foia_df <- foia_df %>%
  arrange(FIPS) %>%  # optional sorting
  group_by(FIPS) %>%
  slice(1) %>%
  ungroup()

# subset to include just a few variables to make it easier going forward
foia_df = subset(foia_df, select = c("FIPS", "State", "County.x", "county_state", "Accept.I.247A", "month_247a_county", "always_treated", 'never_treated', "I247a_id",
                                     "pep_id", "pep_month"))

######## pairing with the wage and employment data #############
load("../data/nhgis_qcew_trac_employ.Rdata")
load("../data/nhgis_qcew_trac_wages.Rdata")

full_df_emloy <- merge(foia_df, trac_employment, by.x = "FIPS", by.y = "area_fips", all.y = T)
full_df_wages <- merge(foia_df, trac_wages, by.x = "FIPS", by.y = "area_fips", all.y = T)














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

