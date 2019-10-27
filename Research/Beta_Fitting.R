library(gamlss)

#Fit Beta to each Player's 2017-2018 Season
df_prior <-
  Master_Table %>% 
  filter(Season == "Regular_1516" & Season_Points > 10) %>% 
  group_by(Team, Player) %>% 
  summarise(Points = sum(PTS),
            Team_Goals = sum(Team_Goals_Game)) %>% 
  mutate(p_hat = Points/Team_Goals)

players_prior <- unique(df_prior$Player)

df_prior_sid <-
  Master_Table %>% 
  filter(str_detect(Season, 'Regular'), 
         Player == "Connor McDavid") %>% 
  group_by(Season) %>% 
  summarise(Points = sum(PTS),
            Team_Goals = sum(Team_Goals_Game),
            prop = Points/Team_Goals)

ggplot(data = df_prior, aes(x = p_hat)) +
  geom_histogram()

  ll <- function(alpha, beta) {
    x <-  df_prior_sid$Points #df_prior$Points
    total <- df_prior_sid$Team_Goals#df_prior$Team_Goals
    -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
  }
  
  m_sid <- mle(ll, 
           start = list(alpha = 1, beta = 10), 
           method = "L-BFGS-B", 
           lower = c(0.0001, .1))
  
df_post <-
  Master_Table %>% 
  filter((Season == "Regular_1617" | Season == "Regular_1718") & 
         (Player %in% as.vector(players_prior) | Player == "Connor McDavid"))%>% 
  group_by(Team, Player) %>% 
  summarise(Points = sum(PTS),
            Team_Goals = sum(Team_Goals_Game), 
            Season_Avg_Pts_p_Goal = mean(Pts_Goals_Prop_Season)) %>% 
  mutate(alpha_prior = coef(m)[1],
         beta_prior = coef(m)[2],
         p_prior = alpha_prior/(alpha_prior + beta_prior),
         alpha_post = alpha_prior + Points, 
         beta_post = beta_prior + Team_Goals - Points, 
         p_post = alpha_post/(alpha_post + beta_post))
  

test_compare_p_old <- 
  Master_Table %>% 
  filter(Season == "Regular_1819") %>% 
  select(Player, Pts_Goals_Prop_Season, PTS, Team_Goals_Game, Cumulative_Game) %>% 
  inner_join(
    df_post %>% 
      select(Player, alpha_post, beta_post, p_post)
  )

beta_df <- 
  test_compare_p

fit_nhl <- gamlss(cbind(Points, Team_Goals - Points) ~ Player,
                  data = df_prior,
                  family = BB(mu.link = "identity"))


ggplot(as.data.frame(p), aes(, dbeta(p, 0.1, 0.1), ylab="density", type ="l", col=4))


ggplot(test_compare_p, aes(x = p_post, y = Pts_Goals_Prop_Season)) +
  geom_density_2d()

ggplot(data = df_post, aes(p_post)) +
  stat_ecdf()
  
#Try Method of Moments 


test <- 
  Master_Table %>% 
  filter(Shots_p_Gm > 3, PIM_p_Gm > 1, str_detect(Season, 'Regular')) %>% 
  distinct(Player, Season)

x <- seq(1, 100, 1)

sum(sum((x)))^2 - sum(x^2)
    

