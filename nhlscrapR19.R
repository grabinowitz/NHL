library(nhlscrapr)
library(tidyverse)
gcodes <- seq(20001, 21082, 1)

scrape <- function(gcodes){
  tmp <- list()
  
  for(i in 1:length(gcodes)){
    tmp[[i]] <-retrieve.game(season="20192020", gcode = as.character(gcodes[i]))
  }
  return(tmp)
}

NHL1920 <- scrape(gcodes)

saveRDS(NHL1920, "Data/NHL1920.rds")

list_to_pbp <- function(J){
 J[[1]]$refdate <- toString(J[["date"]])
 X <- J[[1]][-c(27:28)]
 X <- mutate_if(X, is.factor, as.character)

 return(X)
}



nhl1920 <- NHL1920

nhl1920_pbp <- lapply(NHL1920, list_to_pbp)

nhl1920_pbp <- lapply(nhl1920_pbp, function(x) (if(ncol(x) < 35) NULL else x))


nhl1920_pbp <- do.call(rbind, nhl1920_pbp)

nhl1920_pbp <- 
  nhl1920_pbp %>% 
  mutate(Win_P1 = ifelse(seconds == 1200 & away.score > home.score, 1, 
                         ifelse(seconds == 1200 & away.score < home.score, 2, 0))) %>% 
  group_by(gcode) %>% 
  mutate(Win_P1 = max(Win_P1))

###################################
###Save csv/RDS & Load to Github###
###################################

write.csv(nhl1920_pbp, "Data/nhl1920_pbp.csv")
saveRDS(nhl1920_pbp, "Data/nhl1920_pbp.RDS")

##### csv & RDS Save

Teams60 <-
  nhl1920_pbp %>% 
  filter(seconds <= 3600) %>% 
  mutate(G = ifelse(etype == "GOAL", 1, 0), 
         SH = ifelse(etype %in% c("GOAL", "SHOT"), 1, 0),
         M = ifelse(etype == "MISS", 1, 0),
         H = ifelse(etype == "HIT", 1, 0),
         PEN = ifelse(etype == "PENL", 1, 0)) %>% 
  group_by(season, gcode, awayteam, hometeam,  refdate, ev.team, Win_P1) %>% 
  summarise(Goals = sum(G),
            Shots = sum(SH),
            Misses = sum(M),
            Hits = sum(H),
            Penalties = sum(PEN)) %>% 
  filter(!(ev.team %in% c("", "Per", "Ear", "Lea"))) %>% 
  group_by(ev.team) %>% 
  mutate(Game_Number = row_number()) 

Home <- 
  Teams60 %>% 
  filter(hometeam == ev.team) %>% 
  ungroup() %>% 
  mutate(ev.team = awayteam,
         Goals_Against = Goals,
         Shots_Against = Shots,
         Misses_Against = Misses,
         Hits_Against = Hits, 
         Penalties_Against = Penalties) %>% 
  select(gcode, ev.team, Goals_Against, Shots_Against, Misses_Against, Hits_Against, Penalties_Against)

Away <- 
  Teams60 %>% 
  filter(awayteam == ev.team) %>% 
  ungroup() %>% 
  mutate(ev.team = hometeam,
         Goals_Against = Goals,
         Shots_Against = Shots,
         Misses_Against = Misses,
         Hits_Against = Hits, 
         Penalties_Against = Penalties) %>% 
  select(gcode, ev.team, Goals_Against, Shots_Against, Misses_Against, Hits_Against, Penalties_Against)


Teams60 <- left_join(Teams60, bind_rows(Home, Away))

Teams60 <- 
  Teams60 %>%          
  rename(Team = ev.team) %>% 
  mutate(Opp = ifelse(Team == awayteam, hometeam, awayteam),
         Avg_Shots = ifelse(Game_Number ==1, 0, (cumsum(Shots) - Shots)/(Game_Number - 1)),
         Avg_Shots_Agst = ifelse(Game_Number ==1, 0, (cumsum(Shots_Against) - Shots_Against)/(Game_Number - 1)),
         Avg_Goals = ifelse(Game_Number ==1, 0, (cumsum(Goals) - Goals)/(Game_Number - 1)),
         Avg_Goals_Agst = ifelse(Game_Number ==1, 0, (cumsum(Goals_Against) - Goals_Against)/(Game_Number - 1)),
         Avg_Misses = ifelse(Game_Number ==1, 0, (cumsum(Misses) - Misses)/(Game_Number - 1)),
         Avg_Misses_Agst = ifelse(Game_Number ==1, 0, (cumsum(Misses_Against) - Misses_Against)/(Game_Number - 1)),
         Avg_Hits = ifelse(Game_Number ==1, 0, (cumsum(Hits) - Hits)/(Game_Number - 1)),
         Avg_Hits_Agst = ifelse(Game_Number ==1, 0, (cumsum(Hits_Against) - Hits_Against)/(Game_Number - 1)),
         Avg_Penalties = ifelse(Game_Number ==1, 0, (cumsum(Penalties) - Penalties)/(Game_Number - 1)),
         Avg_Penalties_Agst = ifelse(Game_Number ==1, 0, (cumsum(Penalties_Against) - Penalties_Against)/(Game_Number - 1)))


Opp <- 
  Teams60 %>% 
  select(season, Team, gcode, Avg_Shots, Avg_Shots_Agst, Avg_Goals, Avg_Goals_Agst, 
         Avg_Misses, Avg_Misses_Agst, Avg_Penalties, Avg_Penalties_Agst, Avg_Hits, Avg_Hits_Agst) %>% 
  rename(Opp = Team,
         Avg_Shots_Opp = Avg_Shots,
         Avg_Shots_Agst_Opp = Avg_Shots_Agst, 
         Avg_Goals_Opp = Avg_Goals, 
         Avg_Goals_Agst_Opp = Avg_Goals_Agst, 
         Avg_Misses_Opp = Avg_Misses,
         Avg_Misses_Agst_Opp = Avg_Misses_Agst,
         Avg_Hits_Opp = Avg_Hits,
         Avg_Hits_Agst_Opp = Avg_Hits_Agst,
         Avg_Penalties_Opp = Avg_Penalties,
         Avg_Penalties_Agst_Opp = Avg_Penalties_Agst)
         

Teams <- 
  left_join(Teams60, Opp) %>% 
  mutate(Loc = ifelse(hometeam == Team, "Home", "Away"))

#PostProcessing - Team Shots wins, P1 win, etc..
test <-
  Teams %>% 
  mutate(Shots_Winner = ifelse(Shots == Shots_Against, "Tie", 
                               ifelse(Shots > Shots_Against, "Yes", "No")),
         Win_P1_Desc = ifelse(Win_P1 == 1 & Loc ==  "Away", "Yes",
                              ifelse(Win_P1 == 2 & Loc == "Home", "Yes", 
                                     ifelse(Win_P1 == 0, "Tie", "No")))) %>% 
  group_by(gcode) %>% 
  fill(Shots_Winner)

ggplot(filter(Shots_60, Game_Number > 10), aes(x = , y = Shots)) +
  geom_point() + 
  geom_line()

test <- filter(Teams60, Game_Number > 10)

cor(test$Avg_Hits, test$Hits)

Teamsglm <- filter(Teams, Game_Number > 10)

shotsglm <- glm(Shots ~ Avg_Shots + Avg_Shots_Agst_Opp + Avg_Misses + Loc, family = "poisson", data = Teamsglm)

hitsglm <- glm(Hits ~ Avg_Hits + Avg_Hits_Agst_Opp + Loc, family = "poisson", data = Teams)
  
goalsglm <- glm(Goals ~ Avg_Goals + Avg_Shots_Agst_Opp + Loc, family = "poisson", data = Teamsglm)

ggplot(Shots_60, aes(x = Shots, y = Penalties)) + 
  geom_point()




#Are there more Hits in Boston and LA
avg_hits <- function(df, team){
  team <- enquo(team)
  
  tmp <-
    df %>% 
    group_by(Team) %>% 
    summarise(Avg_Hits = mean(Hits))
  return(tmp)

}

avg_hits(Teams60, COL)

avg <- function(df, group_var, stat){
  p <- 
    df %>% 
    group_by({{ group_var }}) %>% 
    summarise("Avg_{{ stat }}" := mean({{ stat }})) %>% 
    ggplot(aes(x = {{ group_var }}, y = "Avg_{{ stat }}")) + 
    geom_point()
  
  return(p)
}

avg(Teams60, Team, Goals)

test <-avg(Teams60, Team, Goals)

y <- Nsh_Chi[A == H][, A := A + 1]

