
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
Start_Date = "2017-10-01" 
End_Date   = "2019-10-01"   
###########################

source("Scrape_Fns.R")





NHL_15today <- UpdateStatsTable(NHL_15today)
Master_Table_15today <- PostProcess(NHL_15today)  


Master_Table_15today %>% 
  filter(Season == "Regular_1920") %>% 
  group_by(Season, Player) %>% 
  summarise(Total_Points = mean(Season_Points)) %>% 
  arrange(desc(Total_Points))
