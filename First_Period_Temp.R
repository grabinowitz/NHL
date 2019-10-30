library(rvest)
library(tidyverse)

link <- "https://www.hockey-reference.com/boxscores/201910100COL.html"


Periods <-
  link %>% 
  read_html() %>% 
  html_nodes(xpath = "//table") 

DF2 <- Periods[[1]] %>% html_table()

if(sum(colnames(DF2) == rep("1st Period", 5)) == 5){
  colnames(DF2) <- c("Time", "Team", "Type", "Scorer", "Assists")
  DF2 <- 
    DF2 %>% 
    mutate(Period_1 = 1, 
           Period_2 = ifelse(Time == "2nd Period", 2, NA),
           Period_3 = ifelse(Time == "3rd Period", 3 , NA))
  
  DF2$Period <- coalesce(FillDown(Var = DF2$Period_3), FillDown(Var = DF2$Period_2), DF2$Period_1)
  
}

max(DF2$Period_1, DF2$Period_2)

DF2 %>% 
  mutate(Period = ifelse(Period_3 == 3, 3,
                         ifelse(is.na(Period_3) & Period_2 == 2, 2, 
                                1)))
colnames(DF2) <- c("Time", "Team", "Type", "Scorer", "Assists")

DF2 <-
  mutate(Period = ifelse(str_detect(DF2$Time, "Period") & str_detect(DF2$Time, "1st"), 
                         ifelse(Time == "2nd Period", 1, 
                                ifelse()
                                ifelse(Time == "3rd Period, "))))



DF <- data.table(html_table(Periods[[1]]))
colnames(DF) <- c("Time", "Team", "Type", "Scorer", "Assists")
DF[,Period := cumsum(
  ifelse(str_detect(str_detect(DF2$Time, "Period") & str_detect(DF2$Time, "1st")),
         ifelse(Time == "2nd Period", 1, ifelse(Time == "3rd Period", 1, 0))) + 1,]

DF <- DF[!grepl("Period", DF$Time), ,]
DF[, Goals := count(P), by = c("Period", "T")]

