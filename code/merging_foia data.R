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

# wd ----
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")

# loading the three datasets ----
I247a <- read.csv("../data/foia/I247a.csv")
pep <- read.csv("../data/foia/PEP_2015.csv")
df <- read.csv("../data/foia/map.csv")

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
  "Jail.or.Prison.Type", 
  "Month.and.Year.began.accepting.I.247A"                                                        
))]

# If not already in Date format, convert the date column
# Convert the date column from day/month/year format to Date class
I247a$month_I247a <- as.Date(I247a$month_I247a, format = "%d/%m/%Y")
I247a$month_I247a <- as.yearmon(I247a$month_I247a)
#switching blanks to NA
I247a$month_I247a [I247a$month_I247a  == ""] <- NA

# cleaning large df ----

#dropping 0s
df <- df[df$FORMATTED.FIPS != 0, ]

#padding out the values with 0s
df$FORMATTED.FIPS  <- str_pad(df$FORMATTED.FIPS, width = 5, side = "left", pad = "0")

# merging the large df with the i247a dataframe ----

# 1. Function to find the closest matching county in df for each county in detainers
find_closest_county <- function(county_name, county_list) {
  distances <- stringdist::stringdist(county_name, county_list, method = "jw")  # Jaro-Winkler distance
  closest_match <- county_list[which.min(distances)]  # Find the county with the smallest distance
  return(closest_match)
}

# 2. Apply fuzzy matching to the 'County' column in detainers dataset
I247a$matched_county <- sapply(I247a$County, function(x) find_closest_county(x, df$County))

# 3. Merge on both 'State' and the 'matched_county' column
df_I247a <- left_join(I247a, df, by = c("State" = "State", "matched_county" = "County"))

# there are multiple jails in a county - > if any of the jails participate in the detainer program then 
# detainer county ==1
# so in this case a few duplicates are okay and the rows are functionally the same if I select 
# the first item

#this makes a new colum n
df_I247a <- df_I247a %>%
  group_by(GEOID...FIPS) %>%
  mutate(
    I247_flag = any(Accept_I247A == "YES"),
    I247_date = if (all(is.na(month_I247a))) NA else month_I247a[which.min(month_I247a)]
  ) %>%
  ungroup()

#checking that worked
table(df_I247a$I247_flag)

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

######################## pep ###########################
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


#this makes a new colum n
pep_df <- pep_df %>%
  group_by(GEOID...FIPS) %>%
  mutate(
    no_detainers_flag = as.integer(!is.na(month_no_detainers)),
    first_no_detainers = if (all(is.na(month_no_detainers))) NA else month_no_detainers[which.min(month_no_detainers)]
  ) %>%
  ungroup()
#checking that worked
table(pep_df$no_detainers_flag)
table(pep_df$Declines.287.g..Program)
xtabs(~Declines.ICE.Detention.Contract + no_detainers_flag, data = pep_df) 
  

#getting rid of uneccessary columns
pep_df <- subset(pep_df, select = c("Jail.or.Prison.Type", "County","State", "ICE.Access.to.Jail", "Accept_I247A", "Hold.For.ICE", "month_detainer", "matched_county", "FORMATTED.FIPS"))
#dropping 0s
df_detainers <- df_detainers[df_detainers$FORMATTED.FIPS != 0, ]

#padding out the values with 0s
df_detainers$FORMATTED.FIPS  <- str_pad(df_detainers$FORMATTED.FIPS, width = 5, side = "left", pad = "0")

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

