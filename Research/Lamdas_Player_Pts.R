library(tidyverse)
library(data.table)
library(nloptr)

#Investigate Probabilities of Players outscoring one another

#Connor 1.85, tie 3.65, pettersson, 3.00
#Draisaitl 2.1 tie 3.85 Boeser 3.2

#Reverse the Probabilities of Scores
#Match winner Van 2.05, Edm 1.7
#Over/Under 5.5 - Over - 1.75, Under - 1.95

###Assume Each Team's Goals follow Poisson
###Solve for Lamda_H (X) and Lamda_A (Y)

Reverse_Probs <- function(Away_ML, Home_ML){
  Probs <- c(0, 0)
  Probs[1] <- (1/Away_ML)/(1/Away_ML + 1/Home_ML)
  Probs[2] <- 1 - Probs[1]
  return(Probs)
}

Odds_A_H <- Reverse_Probs(1.9, 1.8)
Odds_O_U_5.5 <- Reverse_Probs(1.6, 2.2)

odds_u_5.5 <- Odds_O_U_5.5[2]
away_win_prob <- Odds_A_H[1]
#Now that we have odds of each team winning, use 


O_U_fn <- function(Lamda){
  Obj <- abs(sum(dpois(0:5, Lamda)) - odds_u_5.5)
  return(Obj)
}

Total_Lamda <- cobyla(5, O_U_fn, 4, 7)$par



Away_Win_Prob <- function(Lamda_A){
  #Create Table of All Possible Scores
  Lamda_H <- Total_Lamda - Lamda_A
  
  scores <- data.table(expand.grid(0:15, 0:15))
  colnames(scores) <- c("A", "H")
  scores[, Prob := dpois(A, Lamda_A) * dpois(H, Lamda_H)]
  
  #Check Away Win Prob
  Away_Win <- scores[A>=H]
  Away_Win[,Prob := ifelse(A > H, Prob, Prob/2)]
  a_win_prob <- sum(Away_Win$Prob)
  
  Obj <- abs(a_win_prob - away_win_prob)
  return(Obj)
  
}

Away_Lamda <- cobyla(2.5, Away_Win_Prob, 2, 4)$par
Home_Lamda <- Total_Lamda - Away_Lamda

#Now have Home & Away Lamdas & Player Pts/Goals.. Can evaluate Binomial model on each possible outcome
#Given a score, calculate the probability: away player outscores, tie, home player outscores

Prob_Away_Outscores <- function(Goals_A, Goals_H, PpG_A, PpG_H){
  PTS <- data.table(expand.grid(0:Goals_A, 0:Goals_H))
  colnames(PTS) <- c("A_Pts", "H_Pts")
  PTS[, `:=`(Prob = dbinom(A_Pts, Goals_A, PpG_A)* dbinom(H_Pts, Goals_H, PpG_H)), ]
  return(c(sum(PTS[A_Pts > H_Pts]$Prob), sum(PTS[A_Pts == H_Pts]$Prob), sum(PTS[A_Pts < H_Pts]$Prob)))
}

Score_Table <- function(Lamba_A, Lamda_H, Player_A, Player_H){
  #Create Table of All Possible Scores
  scores <- data.table(expand.grid(0:12, 0:12))
  colnames(scores) <- c("A", "H")
  scores[, Prob := dpois(A, Lamba_A) * dpois(H, Lamda_H)]
  
  for(i in 1:nrow(scores)){
    temp <- Prob_Away_Outscores(scores[i, 1]$A, scores[i, 2]$H, Player_A, Player_H)
    scores$Away_Win[i] <- temp[1]
    scores$Tie[i] <- temp[2]
    scores$Home_Win[i] <- temp[3]
  }
  
  Away_Player_Win <- sum(scores$Prob * scores$Away_Win)
  Tie <- sum(scores$Prob * scores$Tie)
  Home_Player_Win <- sum(scores$Prob * scores$Home)
  
  
  return(c(Away_Player_Win, Tie, Home_Player_Win))
}

Kane_Giroux <- Score_Table(Away_Lamda, Home_Lamda, 0.41509434, 0.3526971)
Dra_Boe <- Score_Table(Away_Lamda, Home_Lamda, 0.3093923 ,0.4585153)

Toews_Cout <- Score_Table(Away_Lamda, Home_Lamda, 0.3033708, 0.3247863)

