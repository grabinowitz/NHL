
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
<<<<<<< HEAD
Start_Date = "2015-10-01" 
=======
Start_Date = "2017-10-01" 
>>>>>>> 976a0a87262098641b1a42de19be596cb5d706f6
End_Date   = "2019-10-01"   
###########################

source("Scrape_Fns.R")

<<<<<<< HEAD
#Get Data
NHL_1519 <- getStats()

#Write Data
write_csv(NHL_1519, "Data/NHL_1519_Extract.csv")
  

#Read Data if necessary
NHL_Historic <- read_csv("Data/NHL_15to19.csv")

#Add columns
Master_Table <- PostProcess(Update_NHL_Historic)
=======


NHL_1519 <- bind_rows(NHL_1516, NHL_1617, NHL_1719)
write_csv(NHL_1519, "Data/NHL_1519_Extract.csv")
  
NHL_Historic <- read_csv("Data/NHL_15to19.csv")



Master_Table <- PostProcess(Update_NHL_Historic)

colnames(Extract2)

test <- 
  Master_Table %>% 
  select(one_of(colnames(Extract2)))

>>>>>>> 976a0a87262098641b1a42de19be596cb5d706f6
