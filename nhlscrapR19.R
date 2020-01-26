library(nhlscrapr)
gcodes <- seq(20001, 20800, 1)

scrape <- function(gcodes){
  tmp <- list()
  
  for(i in 1:length(gcodes)){
    tmp[[i]] <-retrieve.game(season="20192020", gcode = as.character(gcodes[i]))
  }
  return(tmp)
}

NHL1920 <- scrape(gcodes)
saveRDS(NHL1920, "NHL1920.rds")

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

Team_Shots60 <-
nhl1920_pbp %>% 
  mutate(G = ifelse(etype == "GOAL", 1, 0), 
         SH = ifelse(etype %in% c("GOAL", "SHOT"), 1, 0),
         M = ifelse(etype == "MISS", 1, 0),
         H = ifelse(etype == "HIT", 1, 0),
         PEN = ifelse(etype == "PENL", 1, 0)) %>% 
  group_by(season, gcode, refdate, ev.team) %>% 
  summarise(Goals = sum(G),
            Shots = sum(SH),
            Misses = sum(M),
            Hits = sum(H),
            Penalties = sum(PEN))
  





shots <- 
  test2 %>% 
  filter(etype %in% c("GOAL")) %>% 
  mutate(Game = as.numeric(gcode) - 20000) %>% 
  group_by(Game, awayteam) %>% 
  summarise(Shots = n())


test3 <- 
  test2 %>% 
  mutate(Leading_After_1 == ifelse(period == 1 & away.score > home.score))


ggplot(shots, aes(x = Shots)) + 
  geom_bar()

test1 <- test[[1]]
test3 <- test[[4]]

xyz <- bind_rows(xyz, test3)
