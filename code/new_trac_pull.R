#######################
## TRAC data 
#######################
#if we end up scraping
library('rvest')
#for pulling the graph data
library('jsonlite')
library('stringr')
library('tidyr')
#trac = read_html('https://trac.syr.edu/phptools/immigration/arrest/table')

#for parallel processing
library(foreach)
library('doParallel')

setwd("C:/Users/casem/Box/undoc/trac")

#########################################################################
#### county level arrests, aggregated
options(timeout = 1000) #may need to adjust this depending on the computer

#testing one link before i create a loop
test <- fromJSON("https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number")
(test[[1]])

#new loop - county level apprehensions
county_arrests_list <- list()
for (i in 1:1972){ #this will be the individual counties
  for (j in 1:1){#this will be the types of arrest method
    url <- "https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
    county <-paste0("county=",i) #county (there are 1000 of these)
    #arrest_method <- paste0("&arrest_method=",j)#arrest method (there are 26 of these)
    url = paste0(url, county) #arrest_method #new url
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
    county_arrests_list[[length(county_arrests_list) + 1]] <- df
  }
  print(i) #so i can see how long things take
}
#all arrests for all counties!
county_arrests_all_2_18 <- do.call(rbind, county_arrests_list)
save(county_arrests_all_2_18, file = "county_arrests_all_2_18.Rdata")

#splitting our title and our arrest charge
#county_arrests_all <- separate(county_arrests_all_2_18, title, into = c("county", "arrest"), sep = "-")

#making a row where we get rid of the word "county"
#county_arrests <- separate(county_arrests, county, into = c("county"), sep = ", ")

#county_arrests$county_name <- gsub("County", "", county_annual$county) 
#county_arrests$county_name <- gsub("Municipality", "", county_annual$county_name) 

#save(county_arrests, file = "county_annual.Rdata")

#new loop - county level apprehensions
county_arrests_list_located <- list()
for (i in 1:1972){ #this will be the individual counties
  for (j in 3:3){#this will be the types of arrest method
    url <- "https://tracreports.org/phptools/immigration/arrest/graph.php?stat=count&timescale=fymon&timeunit=number&"
    county <-paste0("county=",i) #county (there are 1000 of these)
    #arrest_method <- paste0("&arrest_method=",j)#arrest method (there are 26 of these)
    url = paste0(url, county) #arrest_method #new url
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
    county_arrests_list_located[[length(county_arrests_list_located) + 1]] <- df
  }
  print(i) #so i can see how long things take
}
#all arrests for all counties!
county_arrests_list_located <- do.call(rbind, county_arrests_list_located)
save(county_arrests_list_2_19, file = "county_arrests_list_2_19.Rdata")

