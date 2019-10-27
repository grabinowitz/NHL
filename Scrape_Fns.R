### Functions Needed for Player Scrape ###

#Create list of Dates between start_date and end_date
getDateList <- function(start_date, end_date){
  temp_list <- list()
  temp_dates <- seq(as.Date(start_date), as.Date(end_date), "days")
  
  temp_list <- list()
  for(i in 1:length(temp_dates)){
    temp_list[[i]] =
      data.frame(Year = substr(temp_dates[i], 1, 4),
                 Month = substr(temp_dates[i], 6, 7),
                 Day = substr(temp_dates[i], 9, 10))
    names(temp_list)[i] <- paste0(substr(temp_dates[i], 1, 4), 
                                  substr(temp_dates[i], 6, 7),
                                  substr(temp_dates[i], 9, 19) 
    )
  }
  return(temp_list)
}




#Write the link to boxscores of all games on each date between start_date and end_date
getDateLinks <- function(x){
  cbind(x, "Link" = paste0("https://www.hockey-reference.com/boxscores/?",
                           "year=",x[["Year"]],
                           "&month=",x[["Month"]],
                           "&day=",x[["Day"]])
  )
  
}



#get link for each boxscore of each game of each day between start_date and end_date
getGameLinks <- function(x){
  links_temp <-
    html_attr(html_nodes(read_html(as.character(x[["Link"]])), "a"), "href") %>% 
    as_tibble() %>% 
    filter(grepl(paste0("/boxscores/",x[["Year"]], 
                        x[["Month"]], 
                        x[["Day"]]), value)==TRUE) %>% 
    as.data.frame()
}


getStats <- function(x){
  boxscore = paste0("https://www.hockey-reference.com", x)
  print(paste0("Getting Stats from ", boxscore))
  
  #Get all tables
  players_extract <-
    boxscore %>% 
    read_html() %>% html_nodes(xpath = "//table")
  
  
  #Get Team Names
  teams_extract <-
    boxscore %>% 
    read_html() %>% html_nodes(xpath = "//div[contains(@class, 'section_heading')]/h2") %>% 
    html_text() %>%
    as_tibble()
  
  #Keep each team and add column for Team Name & Opponent
  stats_t1  <- html_table(players_extract[[3]])
  colnames(stats_t1) <- gsub("Scoring", "", paste0(colnames(stats_t1), stats_t1[1,]))
  stats_t1 = stats_t1[2:(nrow(stats_t1)-1), ]
  
  
  
  
  stats_t2  <- html_table(players_extract[[5]])  
  colnames(stats_t2) <- gsub("Scoring", "", paste0(colnames(stats_t2), stats_t2[1,]))
  stats_t2 = stats_t2[2:(nrow(stats_t2)-1), ]  
  
  
  stats_t1$Team <- c(rep(slice(teams_extract, 3)[[1]], nrow(stats_t1)))
  stats_t1$Opp  <- c(rep(slice(teams_extract, 5)[[1]], nrow(stats_t1)))
  stats_t1$Loc  <- c(rep("Away", nrow(stats_t1)))
  stats_t1$Game <- c(rep(paste0(stats_t1$Team, " @ ", stats_t1$Opp)))
  
  
  stats_t2$Team <- c(rep(slice(teams_extract, 5)[[1]], nrow(stats_t2)))
  stats_t2$Opp  <- c(rep(slice(teams_extract, 3)[[1]], nrow(stats_t2)))
  stats_t2$Loc  <- c(rep("Home", nrow(stats_t2)))
  stats_t2$Game <- c(rep(paste0(stats_t2$Opp, " @ ", stats_t2$Team)))
  
  stats_table <- rbind(stats_t1, stats_t2)

  return(stats_table) 
}


GetStatsTable <- function(){
  
  links_out <- lapply(lapply(getDateList(Start_Date, End_Date), getDateLinks), getGameLinks)
  
  final_list <- list()
  for(i in 1:length(links_out)){
    if(length(links_out[[i]][,]) ==0) next
    final_list[[i]] <- lapply(links_out[[i]][,], getStats)
    final_list[[i]] <- do.call(rbind, final_list[[i]])
    final_list[[i]]$Date <- names(links_out)[i]
    names(final_list)[i] <- names(links_out)[i]
    print(names(links_out)[i])
  }
  
  final_table <- do.call(rbind, final_list)
  return(final_table)
}

UpdateStatsTable <- function(Existing_Table){
  max_date <- max(Existing_Table[["Date"]])
  
  update_start <- paste0(substr(max_date, 1, 4), "-", substr(max_date, 5, 6), "-", substr(max_date, 7, 8))
  update_end <- Sys.Date()
  
  
  links_out <- lapply(lapply(getDateList(update_start, update_end), getDateLinks), getGameLinks)
  
  final_list <- list()
  for(i in 1:length(links_out)){
    if(length(links_out[[i]][,]) == 0) next
    final_list[[i]] <- lapply(links_out[[i]][,], getStats)
    final_list[[i]] <- do.call(rbind, final_list[[i]])
    final_list[[i]]$Date <- names(links_out)[i]
    names(final_list)[i] <- names(links_out)[i]
    
  }
  
  update_table <- do.call(rbind, final_list)
  
  final_table <- 
    bind_rows(Existing_Table, 
                           select(update_table, one_of(colnames(Existing_Table)))) %>% 
    distinct()
    
  
  return(final_table)
}



