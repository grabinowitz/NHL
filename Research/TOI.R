#TOI Over/Under
players <- c("Phillip Danault", "Jordan Weal", "Nate Thompson", "Jordie Benn")
TOI <- c(17, 13, 12, 19)

TOI_lines <- tibble("Player" = players, 
                    "TOI_Line" = TOI)

TOI_Extract <-
  Master_Table %>% 
  filter(Player %in% players) %>% 
  left_join(TOI_lines) %>% 
  group_by(Player, TOI_Line) %>% 
  summarise(Over_Freq = sum(ifelse(TOI > TOI_Line, 1, 0))/n())

Master_Table %>% 
  filter(Player %in% players) %>%
  select(Player, Date, TOI) %>% 
  group_by(Player) %>%
  mutate(Game = rank(Date)) %>% 
  select(-Date) %>% 
  ggplot(aes(x = TOI, color = Player)) +
  geom_density()

