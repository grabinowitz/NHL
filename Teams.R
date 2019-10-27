#Read in Data
Master_Table <- read_csv("/Users/guyrabinowitz/JV/NHL/Scrape/Data/NHL_15to19.csv")

#Team Analysis

Teams <- 
  Master_Table %>% 
  group_by(Season, Date, Team, Opp, Loc) %>% 
  summarise(Shots = sum(S),
            Goals = sum(G), 
            PIM = sum(PIM)) %>% 
  mutate(Game = ifelse(Loc == "Away", 
                       paste0(Team, " @ ", Opp), 
                       paste0(Opp, " @ ", Team))) %>% 
  ungroup() %>% 
  group_by(Season, Team) %>% 
  mutate(Game_Number = row_number()) %>% 
  ungroup()


Teams <- 
  Teams %>% 
  left_join(
    (Teams %>%
      dplyr::select(Season, Date, Game, Team, Goals, Shots, PIM) %>%
      rename(Opp = Team, 
             Goals_Against = Goals,
             Shots_Against = Shots,
             PIM_Against = PIM)
     ), 
    by = c("Season", "Date", "Game", "Opp")
    )



#Add Cumulative Average
Teams <-
  Teams %>% 
  group_by(Season, Team) %>% 
  arrange(Date) %>% 
  mutate(Loc = as.factor(Loc),
         Avg_Shots = ifelse(Game_Number ==1, 0, (cumsum(Shots) - Shots)/(Game_Number - 1)),
         Avg_Shots_Agst = ifelse(Game_Number ==1, 0, (cumsum(Shots_Against) - Shots_Against)/(Game_Number - 1)),
         Avg_Goals = ifelse(Game_Number ==1, 0, (cumsum(Goals) - Goals)/(Game_Number - 1)),
         Avg_Goals_Agst = ifelse(Game_Number ==1, 0, (cumsum(Goals_Against) - Goals_Against)/(Game_Number - 1)),
         Avg_PIM = ifelse(Game_Number ==1, 0, (cumsum(PIM) - PIM)/(Game_Number - 1)),
         Avg_PIM_Agst = ifelse(Game_Number ==1, 0, (cumsum(PIM_Against) - PIM_Against)/(Game_Number - 1)))


Opp <- 
  Teams %>% 
  dplyr::select(Season, Team, Date, Game, Avg_Shots, Avg_Shots_Agst, Avg_Goals,
                Avg_Goals_Agst, Avg_PIM, Avg_PIM_Agst) %>% 
  rename(Opp = Team,
         Avg_Shots_Opp = Avg_Shots,
         Avg_Shots_Agst_Opp = Avg_Shots_Agst, 
         Avg_Goals_Opp = Avg_Goals, 
         Avg_Goals_Agst_Opp = Avg_Goals_Agst, 
         Avg_PIM_Opp = Avg_PIM,
         Avg_PIM_Agst_Opp = Avg_PIM_Agst)
         

Teams <- left_join(Teams, Opp) %>% ungroup()


#Add Win Information
Teams <-
  Teams %>% 
  mutate(Win = ifelse(Goals > Goals_Against, 1, 0),
         Avg_Goal_Diff = Avg_Goals - Avg_Goals_Opp)

Teams <- 
  Teams %>% 
  group_by(Team) %>% 
  mutate(Cumul_Win = cumsum(Win))

#Last Game Result
Winlag <- 
  Teams %>% 
  select(Date, Team, Win) %>% 
  arrange(Date) %>% 
  mutate(Win_Last_Game = lag(Win)) %>% 
  select(-Win)

Teams <- Teams %>% left_join(Winlag)
  
Teams_trn <- filter(Teams, Game_Number >= 10 & Game_Number < 60, Season == "Regular_1718")
Teams_tst <- filter(Teams, Game_Number >= 60, Season == "Regular_1718")   


#plot density of shots

ggplot(Teams %>% filter(str_detect(Season, "Regular")), aes(x = Shots, color = Season)) + 
  geom_density()


glm_pois <- glm(Shots ~ Avg_Shots + Avg_Shots_Agst_Opp + Loc, family = "poisson", data = Teams_trn)
glm_qpois <- glm(Shots ~ Avg_Shots + Avg_Shots_Agst_Opp + Loc, family = "quasipoisson", data = Teams_trn)
glm_lm <- glm(Shots ~ Avg_Shots + Avg_Shots_Agst_Opp + Loc, family = "gaussian", data = Teams_trn)


Hold_out <- tibble("Obs" = Teams_trn$Shots)

Hold_out$Pred_Pois <- predict.glm(glm_pois, Teams_trn, type = "response")
Hold_out$Pred_QuasiPois <- predict.glm(glm_qpois, Teams_trn, type = "response")
Hold_out$Pred_Lm <- predict.glm(glm_lm, Teams_trn, type = "response")

Hold_out <- 
  Hold_out %>% 
  mutate(Gap = Pred_Lm - Pred_Pois)

Hold_out$Likelihood_Pois <- dpois(Hold_out$Obs, Hold_out$Pred_Pois)

Teams_trn$pred <- predict.glm(testglm, type = "response")
test <- Teams_trn %>% group_by(Team) %>% summarise(avg_pred = mean(pred))


OT <- 
  Teams %>% 
    filter(Goals == Goals_Against)
