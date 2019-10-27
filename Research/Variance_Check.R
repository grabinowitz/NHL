#Compare Mean=Var (poisson) with Observed Variance to check for over/under dispersion in Poisson estimate
Var_Check_P <-
  Master_Table %>% 
  group_by(Player) %>% 
  summarise(Games = n(),
            PPG = sum(PTS)/n(),
            Var = var(PTS),
            overdisp  = ifelse(PPG > Var, 1, 0),
            underdisp = ifelse(PPG < Var, 1, 0)
  ) %>% 
  filter(Games >= 30) %>% 
  arrange(PPG) %>% 
  reshape2::melt(c("Player"), c("PPG", "Var"), "Measure")
  

ggplot(Var_Check_P, aes(x = value, color = Measure)) +
  geom_bar()



Var_Check_S <-
  Teams %>% 
  group_by(Team) %>% 
  summarise(Games = n(),
            SPG = sum(Shots)/n(),
            Var = var(Shots),
            overdisp  = ifelse(SPG > Var, 1, 0),
            underdisp = ifelse(SPG < Var, 1, 0)
  ) %>% 
  filter(Games >= 30) %>% 
  arrange(SPG)

ggplot(Var_Check_S, aes(x = SPG, y = Var)) +
  geom_smooth(method = "lm") +
  geom_point() +
  gganimate()



Var_Check_G_Teams <-
  Teams %>% 
  group_by(Team) %>% 
  summarise(Games = n(),
            Avg_Goals = sum(Goals),
            GPG = sum(Goals)/n(),
            Var = var(Goals),
            overdisp  = ifelse(GPG > Var, 1, 0),
            underdisp = ifelse(GPG < Var, 1, 0)
  ) %>% 
  filter(Games >= 30 & GPG > 0) %>% 
  arrange(GPG)


#Predict Points with Poisson
set.seed(1234)

Player_Pts_Pred <- 
  Master_Table %>% 
  select(Player, Team, Season, Cumulative_Game, Cumulative_Points, PTS, Cumul_PPG) %>% 
  group_by(Season, Player, Team) %>% 
  mutate(Pts_Lambda = lag(Cumul_PPG, 1)) %>% 
  filter(Cumulative_Game > 1) %>% 
  mutate(Pred = rpois(1, Pts_Lambda),
         Obs= PTS) %>% 
  group_by(Cumulative_Game) %>% 
  summarise(Avg_Pred = mean(Pred),
            Avg_Obs = mean(Obs),
            Residual = Avg_Pred - Avg_Obs) %>% 
  ggplot(aes(x = Residual)) +
  geom_bar()


