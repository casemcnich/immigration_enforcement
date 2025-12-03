######################################
# the purpose of this is to clean and merge the foia data 
# and the construction industry in
# last modified on 10.5.25

# prerequisites ----
#libraries
library('lubridate')
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
library('did')
library("blscrapeR")

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
I247a$month_247a <- as.Date(I247a$month_247a, format = "%m/%d/%Y")

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

class(df_I247a$month_247a)

df_I247a <- df_I247a %>%
  group_by(FIPS) %>%
  mutate(
    # County-level treatment flag
    always_treated = as.integer(any(always_treated == 1, na.rm = TRUE)),
    
    # Safely compute earliest treatment month (handle all-NA case)
    min_month = if (all(is.na(month_247a))) as.Date(NA) else min(month_247a, na.rm = TRUE),
    
    #  Assign county-level treatment month (numeric)
    month_247a_county = if_else(always_treated == 0, as.Date(min_month), as.Date(NA)),
    
    # Flag counties never treated
    never_treated = ifelse(always_treated == 0 & is.na(min_month), 1L, 0L)
  
  ) %>%
  ungroup()

# fixing dummy variables
# if month_247 is < 2014, then always treated is yes
df_I247a$always_treated[df_I247a$month_247a_county < as.Date("2014-01-01")] <- 1
# if always treated, year = 1999
df_I247a$month_247a_county[df_I247a$always_treated == 1] <- as.Date("1999-01-01") #assign arbitrary date to always treate

#padding out fips
df_I247a$FIPS  <- str_pad(df_I247a$FIPS, width = 5, side = "left", pad = "0")

# formating the date correctly 
df_I247a$month_247a_county <- as.Date(df_I247a$month_247a_county, format = "%d/%m/%Y")

# pep 2015 -----
#delete any row where the state is missing
pep <- pep %>%
  filter(!(nchar(State) == 0 | is.na(State))) %>%
  mutate(pep_id = row_number()) %>%
  #deleting duplicate rows
  distinct()

# format date 
pep$month_no_detainers <- as.Date(pep$month_no_detainers, format = "%d/%m/%Y")

# delete any full state variables or the weird b6 counties
pep <- pep[ 
  !grepl("7|state of|State of|All counties", pep$County, ignore.case = TRUE), 
]

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

# dealing with duplicate county observations
pep_df <- pep_df %>%
  group_by(FIPS) %>%
  mutate(dif_detainers = n_distinct(month_no_detainers) > 1) %>%
  ungroup()

table(pep_df$dif_detainers)

pep_df <- pep_df %>%
  group_by(FIPS) %>%
  mutate(
    # Compute minimum date (even if NA — don't skip)
    county_month_no_detainers = suppressWarnings(max(month_no_detainers, na.rm = TRUE))) %>%  # handle all NA gracefully
  ungroup() 

# merging pep_df and df_I247a -----
foia_df <- merge(df_I247a, pep_df, by = c("FIPS", "State"), all = T)

#* dealing with duplicates by county
# RULE: GO with the observation that has the most aggressive ICE policy (then bias will be down)
# eg: this would be the county where month_no_detainer is the most recent date or never happened

foia_df <- foia_df %>%
  group_by(FIPS) %>%
  mutate(dif_detainers = n_distinct(county_month_no_detainers) > 1) %>%
  ungroup()

# there are no issue observations
table(foia_df$dif_detainers)

# making sure the always_treated flag works
foia_df$month_247a_county[foia_df$always_treated == 1] <- as.Date("1999-01-01") #assign arbitrary date to always treate

#* writing flags of issues ----
foia_df$flag <- ifelse(((!is.na(foia_df$month_no_detainers)| foia_df$month_no_detainers >= "2014-01-01") & foia_df$always_treated == 1), 1, 0)
table(foia_df$flag)

table(foia_df$month_no_detainers)

# dropping anything thats "always treated" but isn't actually always treated
foia_df <- foia_df %>%
  filter(!(always_treated == 1 & !is.na(month_no_detainers) & month_no_detainers > 2014))

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

# 1 problematic counties - NEED TO CHECK THESE LATER
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

# merging - this expands to a panel so this makes sense it gets much larger
full_df_employ <- merge(foia_df, trac_employment, by.x = "FIPS", by.y = "area_fips", all.y = T)
full_df_wages <- merge(foia_df, trac_wages, by.x = "FIPS", by.y = "area_fips", all.y = T)

# calculate share foreign born 
full_df_wages$share_foreign_born <- full_df_wages$aab5e008/full_df_wages$aaa5e001
full_df_employ$share_foreign_born <- full_df_employ$aab5e008/full_df_employ$aaa5e001

# Event study -----
#* cleaning 

  # Calculate the difference in months between 'yearmon' and 'I247_date' as an integer
  full_df_employ <- full_df_employ %>%
  mutate(
    # Create a date from the YEAR and month columns (assume day = 1)
    current_date = make_date(year, month, 1),
    
    # Calculate whole months difference (handles NAs safely)
    months_since_I247 = if_else(
      !is.na(month_247a_county),
      interval(month_247a_county, current_date) %/% months(1),
      NA_integer_
    )
  )

#creating a treatment variable for ever treated and always treated
full_df_employ <- full_df_employ %>% mutate(treat = ifelse(!is.na(c(month_247a_county)), 1, 0)) 

#* arrests event study -----

event_subset <- subset(full_df_employ, month_247a_county > "2014-01-01")

event_subset <- subset(event_subset, months_since_I247 < 20)
event_subset <- subset(event_subset, months_since_I247 > -20)

event_subset$z_score_cound <- (event_subset$count - mean(event_subset$count)) / sd(event_subset$count)
event_subset$z_score_cons <- (event_subset$monthly_emplvl_const - mean(event_subset$monthly_emplvl_const)) / sd(event_subset$monthly_emplvl_const)

event_subset$current_date_factor <- as.factor(event_subset$current_date)

event <- feols(
   count~ i(months_since_I247, treat, ref = -1)+ factor(current_date_factor) + state | County.x,
  data = event_subset,   cluster = ~ County.x)

summary(event)

event_subset$current_date_factor <- as.factor(event_subset$current_date)

iplot(event, main = "I247a signatures on arrests", xlab = "Months Since I247a signed", ylab = "Estimate and 95% conf int",   col = "#800020")

#making demeaned version, z score version, and inverse hyperbolic sine version
event_subset <- event_subset %>%
  group_by(county_state) %>%
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
           mutate( ihs_emp_const = log(monthly_emplvl_const + ((monthly_emplvl_const^2 +1)^0.5))) 

event <- feols(
  ihs_emp_rest ~ i(months_since_I247, treat, ref = -1)  + factor(current_date_factor) + state | county_state,
  data = event_subset,   cluster = ~county_state)

summary(event)
iplot(event)

event <- feols(
  ihs_emp_const ~ i(months_since_I247, treat, ref = -1)  + factor(current_date_factor) + state | county_state,
  data = event_subset,   cluster = ~county_state)

summary(event)
iplot(event)

event <- feols(
  ihs_emp_other ~ i(months_since_I247, treat, ref = -1)  + factor(current_date_factor) + state | county_state,
  data = event_subset,   cluster = ~county_state)

summary(event)
iplot(event)

# Diff in diff ------
#* arrest -------
full_df_employ$month_247a_county <- as.Date(full_df_employ$month_247a_county)
full_df_employ$current_date <- as.Date(full_df_employ$current_date)

# converting dates and id to numeric 
full_df_employ$current_date <- as.numeric(full_df_employ$current_date)
full_df_employ$FIPS <- as.numeric(full_df_employ$FIPS)
full_df_employ$month_247a_county <- as.numeric(full_df_employ$month_247a_county)
# if never treated code month i247a as 0 
full_df_employ$month_247a_county[is.na(full_df_employ$month_247a_county)] <- 0

head(full_df_employ[, c("current_date", "month_247a_county")], 100)

# dropping always treated
full_df_employ <- subset(full_df_employ, always_treated != 1)
full_df_employ$z_count <- scale(full_df_employ$count)

# dropping harris tx 
full_df_employ <- subset(full_df_employ, county_state != "Harris, TX")

# dropping any missing values
full_df_clean <- full_df_employ |> filter(!is.na(state.x))
full_df_clean <- full_df_employ |> filter(!is.na(count))
full_df_clean <- full_df_employ |> filter(!is.na(month_247a_county))
full_df_clean <- full_df_employ |> filter(!is.na(FIPS))
full_df_clean <- full_df_employ |> filter(!is.na(current_date))


out <- att_gt(yname = "count",
              gname = "month_247a_county",
              idname = "FIPS",
              tname = "current_date",
              xformla = ~ state.x,
              data = full_df_employ,
              est_method = "reg", 
              control_group = "nevertreated"
)
group_effects <- aggte(out, type = "group", na.rm = TRUE)
summary(group_effects)

es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(es) +
  ylim(-100, 100)  # only show event times from -6 to +6

table(full_df_employ$state.y, useNA = "always")

#* employment -----
# Calculate Z scores 
full_df_employ$z_monthly_emplvl_const <- as.numeric(scale(full_df_employ$monthly_emplvl_const))
full_df_employ$z_monthly_emplvl_rest <- as.numeric(scale(full_df_employ$monthly_emplvl_rest))
full_df_employ$z_monthly_emplvl_other <- as.numeric(scale(full_df_employ$monthly_emplvl_other))

out <- att_gt(yname = "z_monthly_emplvl_const",
              gname = "month_247a_county",
              idname = "FIPS",
              tname = "current_date",
              xformla = ~1,
              data = full_df_employ,
              est_method = "reg", 
              control_group = "nevertreated"
)
group_effects <- aggte(out, type = "group", na.rm = TRUE)
summary(group_effects)

es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(es) +
  ylim(-10, 10)  

out <- att_gt(yname = "z_monthly_emplvl_rest",
              gname = "month_247a_county",
              idname = "FIPS",
              tname = "current_date",
              xformla = ~ state.x,
              data = full_df_employ,
              est_method = "reg", 
              control_group = "nevertreated"
)
group_effects <- aggte(out, type = "group", na.rm = TRUE)
summary(group_effects)

es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(es) +
  ylim(-10, 10)  

#* wages ----

#** CPI adjustment 

# pull out the monthly CPI
cpi <- bls_api("CUUR0000SA0",
               startyear = 2014,
               endyear = 2018,
               annualavg = FALSE)   # monthly data, not annual averages

# Extract 2014 CPI base (usually you want average or a specific month—here I take annual avg)
cpi$cpi_base <- cpi %>%
  filter(year == 2014) %>%
  summarise(base = mean(value, na.rm = TRUE)) %>%
  pull(base) 

# convert to year quarter
cpi$year_quarter_cpi <- paste0(cpi$periodName, " ", cpi$year)

month_year_to_quarter <- function(x) {
  d <- as.Date(paste("1", x), format = "%d %B %Y")   # for full month names
  q <- ceiling(as.numeric(format(d, "%m")) / 3)
  paste(format(d, "%Y"), paste0("Q", q))
}

cpi$year_quarter_cpi <- month_year_to_quarter(cpi$year_quarter_cpi)

# keep just the beginning of the year quarter value 
cpi <- subset(cpi, cpi$periodName %in% c("January", "April", "July", "Oct"))

# merge with wage data
full_df_wages <- merge(full_df_wages, cpi, by.x = "year_qtr", by.y = "year_quarter_cpi", relationship = "many-to-many")

# adjusted wages
full_df_wages <- full_df_wages %>%
  mutate(rest_wage_real_2014 = avg_wkly_wage_rest * (cpi_base / value)) %>%
  mutate(const_wage_real_2014 = avg_wkly_wage_const * (cpi_base / value)) %>%
  mutate(other_wage_real_2014 = avg_wkly_wage_other * (cpi_base / value))   # 'value' is CPI in the BLS API data

# Convert to numeric first
full_df_wages$year <- as.numeric(full_df_wages$year.y)
full_df_wages$qtr <- as.numeric(full_df_wages$qtr)

# Convert to year.quarter format
full_df_wages$year_quarter <- full_df_wages$year + (full_df_wages$qtr) * 0.25

# converting I247a date
# pulling out year and quarter as numeric 
full_df_wages$month_247a_county <- ymd(full_df_wages$month_247a_county)
full_df_wages$year_quarter_I247a <- lubridate::quarter(full_df_wages$month_247a_county, with_year = TRUE)

# Convert to year.quarter format

# dropping always treated
full_df_wages <- subset(full_df_wages, always_treated != 1)
full_df_wages$z_count <- scale(full_df_wages$count)

# if never treated code month i247a as 0 
full_df_wages$year_quarter_I247a [is.na(full_df_wages$year_quarter_I247a )] <- 0

# dropping harris tx 
full_df_wages <- subset(full_df_wages, county_state != "Harris, TX")

# dropping any missing values
full_df_clean <- full_df_wages %>% filter(!is.na(county_state))

full_df_clean$FIPS <- as.numeric(full_df_clean$FIPS)

table(full_df_clean$State)

full_df_clean$year_quarter_I247a <- full_df_wages$year_quarter_I247a * 10
full_df_clean$year_quarter <- full_df_wages$year_quarter * 10

# calculate z scores 
full_df_clean$z_avg_wkly_wage_const <- as.numeric(scale(full_df_clean$const_wage_real_2014))
full_df_clean$z_avg_wkly_wage_rest <- as.numeric(scale(full_df_clean$rest_wage_real_2014))
full_df_clean$z_avg_wkly_wage_oth <- as.numeric(scale(full_df_clean$other_wage_real_2014))

# drop all counties with a 0 in the wage (drop the full county)
full_df_clean_rest <- full_df_clean[ !full_df_clean$FIPS %in% full_df_clean$FIPS[full_df_clean$rest_wage_real_2014 == 0], ]
full_df_clean_const <- full_df_clean[ !full_df_clean$FIPS %in% full_df_clean$FIPS[full_df_clean$const_wage_real_2014 == 0], ]
full_df_clean_oth <- full_df_clean[ !full_df_clean$FIPS %in% full_df_clean$FIPS[full_df_clean$other_wage_real_2014 == 0], ]

# bootstrapped version
out <- did::att_gt(
  yname = "z_avg_wkly_wage_rest",
  gname = "year_quarter_I247a",
  idname = "FIPS",
  tname = "year_quarter",
  xformla = ~1,
  data = full_df_clean_rest,
  est_method = "reg",
  control_group = "notyettreated",
  bstrap = TRUE,
  biters = 2000,        # <-- correct!
  clustervars = "FIPS"
)

table(full_df_clean$year_quarter_I247a)  # or year_quarter_I247a if that’s your gname


group_effects <- aggte(out, type = "group", na.rm = TRUE)
summary(group_effects)

es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(es) +
  ylim(-1, 1)  



# bootstrapped version
out <- did::att_gt(
  yname = "z_avg_wkly_wage_const",
  gname = "year_quarter_I247a",
  idname = "FIPS",
  tname = "year_quarter",
  xformla = ~1,
  data = full_df_clean_const,
  est_method = "reg",
  control_group = "notyettreated",
  bstrap = TRUE,
  biters = 2000,        # <-- correct!
  clustervars = "FIPS"
)

table(full_df_clean$year_quarter_I247a)  # or year_quarter_I247a if that’s your gname


group_effects <- aggte(out, type = "group", na.rm = TRUE)
summary(group_effects)

es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(es) +
  ylim(-1, 1)  


# bootstrapped version
out <- did::att_gt(
  yname = "z_avg_wkly_wage_oth",
  gname = "year_quarter_I247a",
  idname = "FIPS",
  tname = "year_quarter",
  xformla = ~1,
  data = full_df_clean_oth,
  est_method = "reg",
  control_group = "notyettreated",
  bstrap = TRUE,
  biters = 2000,        # <-- correct!
  clustervars = "FIPS"
)

table(full_df_clean$year_quarter_I247a)  # or year_quarter_I247a if that’s your gname


group_effects <- aggte(out, type = "group", na.rm = TRUE)
summary(group_effects)

es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(es) +
  ylim(-1, 1)  

