Team_PreProcess <- function(DF){
  
  DF <- 
    DF %>% 
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
  
  
  DF <- 
    DF %>% 
    left_join(
      (DF %>%
         dplyr::select(Season, Date, Game, Team, Goals, Shots, PIM) %>%
         rename(Opp = Team, 
                Goals_Against = Goals,
                Shots_Against = Shots,
                PIM_Against = PIM)
      ), 
      by = c("Season", "Date", "Game", "Opp")
    )
  
  
  
  #Add Cumulative Average
  DF <-
    DF %>% 
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
    DF %>% 
    dplyr::select(Season, Team, Date, Game, Avg_Shots, Avg_Shots_Agst, Avg_Goals,
                  Avg_Goals_Agst, Avg_PIM, Avg_PIM_Agst) %>% 
    rename(Opp = Team,
           Avg_Shots_Opp = Avg_Shots,
           Avg_Shots_Agst_Opp = Avg_Shots_Agst, 
           Avg_Goals_Opp = Avg_Goals, 
           Avg_Goals_Agst_Opp = Avg_Goals_Agst, 
           Avg_PIM_Opp = Avg_PIM,
           Avg_PIM_Agst_Opp = Avg_PIM_Agst)
  
  
  DF <- left_join(DF, Opp) %>% ungroup()
  
  
  #Add Win Information
  DF <-
    DF %>% 
    mutate(Win = ifelse(Goals > Goals_Against, 1, 0),
           Avg_Goal_Diff = Avg_Goals - Avg_Goals_Opp)
  
  DF <- 
    DF %>% 
    group_by(Team) %>% 
    mutate(Cumul_Win = cumsum(Win))
  
  #Last Game Result
  Winlag <- 
    DF %>% 
    select(Date, Team, Win) %>% 
    arrange(Date) %>% 
    mutate(Win_Last_Game = lag(Win)) %>% 
    select(-Win)
  
  
  
  DF <- 
    DF %>% 
    left_join(Winlag)

  return(DF)  
}
