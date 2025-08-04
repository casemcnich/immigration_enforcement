
### TRAC CAP vs at large arrest data ###
### if we end up scraping ###
library('rvest')
#for pulling the graph data
library('jsonlite')
library('stringr')
library('tidyr')

#TARGET LINK:
#https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&county=1&community_arrest=1
setwd("C:/Users/casem/Box/undoc/trac")

### county level arrests by arrest type - cap = no ####
options(timeout = 1000) #may need to adjust this depending on the computer
#new loop - county level apprehensions non CAP arrests
county_arrests_nocap <- NULL
for (i in 1:1972){ #this will be the individual counties
    url <- "https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
    county <-paste0("county=",i) #county (there are 397 of these)
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

save(county_arrests_nocap, file = "county_arrests_nocap.Rdata")

### county level arrests by arrest type - cap = no ####
options(timeout = 1000) #may need to adjust this depending on the computer
#new loop - county level apprehensions non CAP arrests
county_arrests_cap <- NULL
for (i in 1:1972){ #this will be the individual counties
  url <- "https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
  county <-paste0("county=",i) #county (there are 397 of these)
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

county_arrests_cap <- county_arrests_cap[, !(names(county_arrests_cap) %in% c("arrest"))]
#renaming the count column
colnames(county_arrests_cap)[colnames(county_arrests_cap) == "count"] <- "cap_arrests"

save(county_arrests_cap, file = "county_arrests_cap.Rdata")


