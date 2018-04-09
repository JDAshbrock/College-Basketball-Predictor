data <- read.table("D:/Users/Jon/Documents/Data Science Projects/March Madness 18/full_data_2017_final.csv", header=TRUE, sep=",")

# We need to scramble the W/L columns to remove dependency on the pre-sorted nature of our predictor
# We only do this for the desired data rows

#Contains a 1 or 2 corresponding to which team won the game
attach(data)
Winning_team <- c()
Team1 <- as.vector(rep(0,5395))
Team2 <- as.vector(rep(0,5395))
Game_num1 <- as.vector(rep(0,5395))
Game_num2 <- as.vector(rep(0,5395))
Avg_score1 <- as.vector(rep(0,5395))
Avg_score2 <- as.vector(rep(0,5395))
Avg_allow1 <- as.vector(rep(0,5395))
Avg_allow2 <- as.vector(rep(0,5395))
Team1_wins <- as.vector(rep(0,5395))
Team2_wins<- as.vector(rep(0,5395))
Scrambled <- data[ ,c(4,6)]
dif <- c() #dif will be score of team 1 below minus score of team 2
home <- c()

for(i in 1:5395){
  rand <- sample(1:2,1)
  if(rand ==1){ #Team1 is assigned to be the winning team
    Team1[i]<-Wteam[i]
    Game_num1[i] <- Wgame_num[i]
    Avg_score1[i] <- Wavg_score[i]
    Avg_allow1[i] <- Wavg_allowed[i]
    Team1_wins[i] <- Wprev_wins[i]
    
    Team2[i]<-Lteam[i]
    Game_num2[i] <- Lgame_num[i]
    Avg_score2[i] <- Lavg_score[i]
    Avg_allow2[i] <- Lavg_allowed[i]
    Team2_wins[i] <- Lprev_wins[i]
    
    Winning_team <- c(Winning_team,1)
    difference <- Wscore[i]-Lscore[i]
    dif <- c(dif, difference)
    if(Wloc[i]=="H"){
      home <- c(home,1)
    } else if(Wloc[i]=="N"){
      home <- c(home,"Neutral")
    }else{
      home <- c(home,2)
    }
    
  }else{ #Team1 is assigned to be the losing team
    Team2[i]<-Wteam[i]
    Game_num2[i] <- Wgame_num[i]
    Avg_score2[i] <- Wavg_score[i]
    Avg_allow2[i] <- Wavg_allowed[i]
    Team2_wins[i] <- Wprev_wins[i]
    
    Team1[i]<-Lteam[i]
    Game_num1[i] <- Lgame_num[i]
    Avg_score1[i] <- Lavg_score[i]
    Avg_allow1[i] <- Lavg_allowed[i]
    Team1_wins[i] <- Lprev_wins[i]
    
    Winning_team <- c(Winning_team,2)
    difference <- Lscore[i]-Wscore[i]
    dif <- c(dif,difference)
    if(Wloc[i]=="H"){
      home <- c(home,2)
    } else if(Wloc[i]=="N"){
      home <- c(home,"Neutral")
    }else{
      home <- c(home,1)
    }
  }
}
home <- as.factor(home)
Winning_team <- as.factor(Winning_team)

Team1_pct <- as.vector(rep(0,length(Team1_wins)))
Team2_pct <- as.vector(rep(0,length(Team2_wins)))

#Compute cumulative winning percentage vector.
for(i in 1:length(Team1_wins)){
  temp1 <- Team1_wins[i]/Game_num1[i]
  temp2 <- Team2_wins[i]/Game_num2[i]
  Team1_pct[i] <- temp1
  Team2_pct[i] <- temp2
}

#define the new columns of our scrambled list
Scrambled$Winning_team <- Winning_team
Scrambled$Team1 <-Team1
Scrambled$Team2 <- Team2
Scrambled$Game_num1 <- Game_num1
Scrambled$Game_num2<-Game_num2
Scrambled$Avg_score1 <- Avg_score1
Scrambled$Avg_score2 <-Avg_score2
Scrambled$Avg_allow1 <- Avg_allow1
Scrambled$Avg_allow2 <- Avg_allow2
Scrambled$dif <- dif
Scrambled$Team1_wins <- Team1_wins
Scrambled$Team2_wins <- Team2_wins
Scrambled$Team1_pct <- Team1_pct
Scrambled$Team2_pct <- Team2_pct
Scrambled$home <- home

write.csv(Scrambled, file="Scrambled_2017.csv", row.names=FALSE)