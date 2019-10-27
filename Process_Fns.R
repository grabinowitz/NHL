### Clean up Data Post Scrape ###

#Convert Character to Numeric
PostProcess <- function(df){
  cols <- c("G","A", "PTS","+/-","PIM",
            "GoalsEV","GoalsPP","GoalsSH","GoalsGW",
            "AssistsEV","AssistsPP","AssistsSH","S")  
  
  for(col in cols){
    df[[col]] <- as.numeric(df[[col]])
  }
  
  #Turn TOI to numeric
  df$TOI <- as.numeric(gsub(":", ".", df$TOI))
  df$TOI <- df$TOI + (df$TOI - floor(df$TOI))*100/60
  
  #Format Date and Add Season
  df <-
    df %>% 
    mutate(Season = ifelse(Date < "2016-04-11", "Regular_1516",
                           ifelse(Date < "2016-10-12", "Playoffs_1516",
                                  ifelse(Date < "2017-04-10", "Regular_1617",
                                         ifelse(Date < "2017-10-04", "Playoffs_1617",
                                                ifelse(Date < "2018-04-09", "Regular_1718",
                                                       ifelse(Date < "2018-10-03", "Playoffs_1718",
                                                              ifelse(Date < "2019-04-10", "Regular_1819",
                                                                     ifelse(Date < "2019-10-04", "Playoffs_1819",
                                                                            "Regular_1920")))))))))
  Master_Table <-
    df %>% 
    mutate(Game_Cnt = 1) %>% 
    group_by(Season, Team, Player) %>% 
    mutate(Cumulative_Goals = cumsum(G),
           Cumulative_Points = cumsum(PTS),
           Cumulative_Game = cumsum(Game_Cnt),
           Cumulative_PIM = cumsum(PIM),
           Cumulative_Shots = cumsum(S),
           G_p_Gm = sum(G)/n(),
           A_p_Gm = sum(A)/n(),
           Pts_p_Gm = sum(PTS)/n(),
           PIM_p_Gm = sum(PIM)/n(),
           Shots_p_Gm = sum(S)/n()) %>% 
    ungroup() %>% 
    group_by(Season) %>% 
    mutate(G_Lg_Avg = sum(G)/n(),
           A_Lg_Avg = sum(A)/n(),
           Pts_Lg_Avg = sum(PTS)/n(),
           PIM_Lg_Avg = sum(PIM)/n(),
           Cumul_PPG = Cumulative_Points/Cumulative_Game,
           Cumul_GPG = Cumulative_Goals/Cumulative_Game,
           Cumul_SPG = Cumulative_Shots/Cumulative_Game
    ) %>% 
    mutate(Rel_Goals   = G_p_Gm/G_Lg_Avg,
           Rel_Assists = A_p_Gm/A_Lg_Avg,
           Rel_PIM     = PIM_p_Gm/PIM_Lg_Avg)
  
  
  
  
  #Calculate each Player's Proportion of Team's Points by game
  #Team x Game Calcs
  Master_Table <- 
    Master_Table %>% 
    group_by(Season, Team, Date) %>% 
    mutate(Team_Goals_Game = sum(G)) %>% 
    ungroup()
  
  
  
  #Player x Season Calcs
  Master_Table <-
    Master_Table %>% 
    group_by(Season, Team, Player) %>% 
    mutate(Pts_Goals_Prop_Season = sum(PTS)/sum(Team_Goals_Game),
           Season_Goals = sum(G),
           Season_Points = sum(PTS),
           Season_Games = n()) %>% 
    ungroup() 
  
  #Final Calcs
  Master_Table <-
    Master_Table %>% 
    mutate(Pts_Goals_Prop_Game = ifelse(Team_Goals_Game == 0, 0, PTS/Team_Goals_Game))
  
  
  
  Master_Table %>% 
    group_by(Player, Season) %>% 
    summarise(Games = n()) %>% 
    arrange(desc(Games))
  
  return(Master_Table)
  
  
}
