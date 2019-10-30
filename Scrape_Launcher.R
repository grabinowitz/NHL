
#Use Hockey Reference to find total shots on a given date
library(rvest)
library(tidyverse)



all.teams <-
  c("Anaheim Ducks", "Arizona Coyotes","Boston Bruins","Buffalo Sabres","Calgary Flames","Carolina Hurricanes",
    "Chicago Blackhawks","Colorado Avalanche","Columbus Blue Jackets","Dallas Stars","Detroit Red Wings","Edmonton Oilers",
    "Florida Panthers","Los Angeles Kings","Minnesota Wild","Montreal Canadiens","Nashville Predators","New Jersey Devils",
    "New York Islanders","New York Rangers","Ottawa Senators","Philadelphia Flyers","Pittsburgh Penguins","San Jose Sharks",
    "St. Louis Blues","Tampa Bay Lightning","Toronto Maple Leafs","Vancouver Canucks","Vegas Golden Knights",
    "Washington Capitals","Winnipeg Jets"
  )



all.teams.list <- as.list(all.teams)

##### INPUTS ##############
Start_Date = "2015-10-01" 
End_Date   = "2019-10-01"   
###########################

source("Scrape_Fns.R")
source("Process_Fns.R")

#Get Data
NHL_1519 <- getStats()


#Read Data if necessary
Extract <- data.table::fread("Data/NHL_1519_Extract.csv", )


#Add columns
Master_Table <- PostProcess(Extract)
