#######################
## TRAC data - NOT IN USE ANYMORE
#######################
#if we end up scraping
library('rvest')
#for pulling the graph data


library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(progress)
library(future.apply)

#trac = read_html('https://trac.syr.edu/phptools/immigration/arrest/table')

setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement/code")

### county level arrests by arrest method ####
plan(multisession, workers = 16) # adjust workers based on your CPU

# county and arrest method parameters
county_ids <- 1:2        # Use 1:397 for full run
arrest_methods <- 1:25

param_grid <- expand.grid(county = county_ids, method = arrest_methods)

fetch_data <- function(row) {
  i <- row["county"]
  j <- row["method"]

  url <- paste0(
    "https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&",
    "county=", i,
    "&arrest_method=", j
  )
  
  # Try to parse the JSON
  json <- tryCatch({
    fromJSON(url)
  }, error = function(e) {
    warning(paste("Error on county:", i, "method:", j))
    return(NULL)
  })
  
  if (is.null(json) || length(json) < 4 || is.null(json[[4]]$number)) {
    return(NULL)
  }
  
  # Return structured data frame
  data.frame(
    title = json[[1]],
    month = json[[4]]$fymon,
    count = json[[4]]$number,
    percent = json[[4]]$percent,
    stringsAsFactors = FALSE
  )
}

# Run the fetch function in parallel
results_list <- future_lapply(1:nrow(param_grid), function(idx) {
  fetch_data(param_grid[idx, ])
})

# Combine and clean results
county_arrests <- bind_rows(results_list)
county_arrests <- separate(county_arrests, title, into = c("county", "arrest"), sep = "-")
  
  
  
  
#new loop - county level apprehensions
county_arrests <- NULL
for (i in 1:2){ #this will be the individual counties
  for (j in 1:25){#this will be the types of arrest method
    url <- "https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
    county <-paste0("county=",i) #county (there are 397 of these)
    arrest_method <- paste0("&arrest_method=",j)#arrest method (there are 25 of these)
    url = paste0(url, county, arrest_method) #new url
    print(url) #so i can see how long things take
    #&county=i&arrest_method=j
    json <- fromJSON(url) #this pulls everything from the individual json and sets 
    month <- as.data.frame(json[[4]]$fymon) #month)
    count <- as.data.frame(json[[4]]$number) #arrest number
    percent <-as.data.frame(json[[4]]$percent) #arrest percent
    title <- json[[1]] #pulling out the county name and arrest percent
    if (length(count)==0) {
      next     #if the dataframe is null then skip
    }
    #piecing together the df
    df <- data.frame(title, month, count, percent)
    colnames(df) <- c( "title","month", "count", "pekrcent") #adding titles
    #this is pulling a one element title
    #binding to our dataframe
    county_arrests <- rbind(county_arrests, df)
  }
}
#splitting our title and our arrest charge
county_arrests <- separate(county_arrests, title, into = c("county", "arrest"), sep = "-")

#########################################################################
#### county level arrests, aggregated
options(timeout = 1000) #may need to adjust this depending on the computer




































test <- fromJSON("https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fy&timeunit=number&county=1")
(test[[1]])


#new loop - county level arrests
county_annual <- NULL
for (i in 1:397){ #this will be the individual counties
    url <- "https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fy&timeunit=number&"
    county <-paste0("county=",i) #county (there are 397 of these)
    url = paste0(url, county) #new url
    print(url) #so i can see how long things take
    json <- fromJSON(url) #this pulls everything from the individual json and sets 
    year <- as.data.frame(json[[4]]$fy) #month)
    count <- as.data.frame(json[[4]]$number) #arrest number
    percent <-as.data.frame(json[[4]]$percent) #arrest percent
    title <- json[[1]] #pulling out the county name
    if (length(count)==0) {
      next     #if the dataframe is null then skip
    }
    #piecing together the df
    df <- data.frame(title, year,count, percent)
    colnames(df) <- c( "title","year", "count", "percent") #adding titles
    #this is pulling a one element title
    #binding to our dataframe
    county_annual <- rbind(county_annual, df)
}


#breaking out county and state
county_annual <- separate(county_annual, title, into = c("county", "state"), sep = ", ")

#making a row where we get rid of the word "county"
county_annual <- separate(county_annual, county, into = c("county"), sep = ", ")

county_annual$county_name <- gsub("County", "", county_annual$county) 
county_annual$county_name <- gsub("Municipality", "", county_annual$county_name) 

save(county_annual, file = "county_annual.Rdata")

#breaking out county and state
county_totals <- separate(county_totals, title, into = c("county", "state"), sep = ", ")
#making a row where we get rid of the word "county"
county_totals <- separate(county_totals, county, into = c("county"), sep = ", ")

county_totals$county_name <- gsub("County", "", county_totals$county) 
county_totals$county_name <- gsub("Municipality", "", county_totals$county_name) 

save(county_totals, file = "county_totals.Rdata")
load("county_totals.Rdata")
load("county_annual.Rdata")

# graveyard but selectors might be helpful in the future (>?)
################################################
################################################
#####################################
# county level x charge
county_charge_df <- NULL
for(i in 1:3){
  url <- glue::glue("https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&county=[i]&arrest_method=2&timeunit=number")
  i <- jsonlite::fromJSON(url)
  county_charge_df <- rbind(county_charge_df, i)
}

######################################
# this one works= city level arrests
out <- NULL
for(i in 1:397){
  url <- glue::glue("https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&depart_city={i}&timeunit=number")
  j <- jsonlite::fromJSON(url)
  tm <- j$timeline
  tm$city <- j$title
  out <- rbind(out, tm)
}

#state level data
state_trac <- NULL
for(i in 1:3){
  url <- glue::glue("https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&state=[]&timeunit=number")
  k <- jsonlite::fromJSON(url)
  tm <- k$timeline
  tm$state_trac <- k$title
  state_trac <- rbind(state_trac, tm)
}


###########################################################
#casey url that worked
#https://trac.syr.edu/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&depart_state=9&arrest_method=2&timeunit=number&