library(rvest)

link <- "https://www.hockey-reference.com/boxscores/201910040PHI.html"


Periods <-
  link %>% 
  read_html() %>% 
  html_nodes(xpath = "//table") 

DF <- data.table(html_table(Periods[[1]])[,1:2])
colnames(DF) <- c("P", "T")
DF[,Period := cumsum(ifelse(P == "2nd Period", 1, ifelse(P == "3rd Period", 1, 0))) + 1,]
DF <- DF[!grepl("Period", DF$P), ,]
DF[, Goals := count(P), by = c("Period", "T")]

SBR_test <-
  link %>% 
  read_html() %>% 
  html_nodes(xpath = "//div[3]/div[5]/div[2]/div/div/div[2]/div/div/div/div/section[1]/main/div[1]/span") 


HR <- "https://www.hockey-reference.com/boxscores/201910030COL.html"

teams_extract <-
  HR %>% 
  read_html() %>% html_nodes(xpath = "//div[contains(@class, 'section_heading')]/h2") %>% 
  html_text() %>%
  as_tibble()
