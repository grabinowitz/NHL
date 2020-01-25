library(nhlscrapr)
gcodes <- seq(20001, 20761, 1)

scrape <- function(gcodes){
  tmp <- list()
  
  for(i in 1:length(gcodes)){
    tmp[[i]] <-retrieve.game(season="20192020", gcode = as.character(gcodes[i]))
  }
  return(tmp)
}

NHL1920 <- scrape(gcodes)

list_to_pbp <- function(J){
 J[[1]]$refdate <- toString(J[["date"]])
 X <- J[[1]][-c(27:28)]
 X <- mutate_if(X, is.factor, as.character)

 return(X)
}

saveRDS(NHL1920, "NHL1920.rds")

nhl1920 <- NHL1920[1:759]


test <- lapply(nhl1920, list_to_pbp)
test23 <- lapply(test, function(x) (if(ncol(x) < 35) NULL))

for(i in 1:length(test)){
  if(length(test[[i]]) < 35)
    test[[i]] <- NULL
}

test2 <- do.call(rbind, test)

test2 %>% 
  filter(etype %in% c("GOAL", "SHOT") & grepl("OVECHKIN", ev.player.1))

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
