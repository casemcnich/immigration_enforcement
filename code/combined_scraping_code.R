# this is the most up to date scraping file 
# there are other files that parallelize this process but were unreliable
# this creates the data sets county_arrests_nocap, county_arrests_cap, and county_arrests

# prerequisits ----
library('dplyr')
library('tidyr')
# if we end up scraping
library('rvest')
# for pulling the graph data
library('jsonlite')
library('stringr')
library('tidyr')
# for parallel processing
library('foreach')
library('doParallel')
library('purrr')
library('progress')
library('future.apply')

### county level arrests by arrest total
options(timeout = 1000) #may need to adjust this depending on the computer
#new loop - county level apprehensions non CAP arrests
county_arrests <- NULL
for (i in 1:1972){ #this will be the individual counties
  url <- "https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
  county <-paste0("county=",i) #county (there are 397 of these)
  url = paste0(url, county) #new url
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
  colnames(df) <- c( "title","month", "count", "percent") #adding titles
  #this is pulling a one element title
  #binding to our dataframe
  county_arrests <- rbind(county_arrests, df)
}
save(county_arrests, file = "../data/county_arrests.Rdata")


### county level arrests by arrest type - cap = no ####

#TARGET LINK:
#https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&county=1&community_arrest=1

options(timeout = 1000) #may need to adjust this depending on the computer
#new loop - county level apprehensions non CAP arrests
county_arrests_nocap <- NULL
for (i in 1:1972){ #this will be the individual counties
  url <- "https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
  county <-paste0("county=",i)
  url = paste0(url, county, "&community_arrest=1") #new url
  print(url) #so i can see how long things take
  #&county=i&arrest_method=j
  json <- fromJSON(url) #this pulls everything from the individual json and sets 
  month <- as.data.frame(json[[4]]$fymon) #month)
  count <- as.data.frame(json[[4]]$number) #arrest number
  percent <-as.data.frame(json[[4]]$percent) #arrest percent
  title <- json[[1]] #pulling out the county name a`nd arrest percent
  if (length(count)==0) {
    next     #if the dataframe is null then skip
  }
  #piecing together the df
  df <- data.frame(title, month, count, percent)
  colnames(df) <- c( "title","month", "count", "percent") #adding titles
  #this is pulling a one element title
  #binding to our dataframe
  county_arrests_nocap <- rbind(county_arrests_nocap, df)
}


county_arrests_nocap <- county_arrests_nocap[, !(names(county_arrests_nocap) %in% c("arrest"))]
#renaming the count column
colnames(county_arrests_nocap)[colnames(county_arrests_nocap) == "count"] <- "no_cap_arrests"

save(county_arrests_nocap, file = "../date/county_arrests_nocap.Rdata")

# county level arrests by arrest type - cap = yes -----
options(timeout = 1000) #may need to adjust this depending on the computer
#new loop - county level apprehensions CAP arrests
county_arrests_cap <- NULL
for (i in 1:1972){ #this will be the individual counties
  url <- "https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
  county <-paste0("county=",i)
  url = paste0(url, county, "&community_arrest=2") #new url
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
  colnames(df) <- c( "title","month", "count", "percent") #adding titles
  #this is pulling a one element title
  #binding to our dataframe
  county_arrests_cap <- rbind(county_arrests_cap, df)
}

save(county_arrests_nocap, file = "../data/county_arrests_cap.Rdata")
