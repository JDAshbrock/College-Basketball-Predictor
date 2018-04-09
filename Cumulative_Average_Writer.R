require("e1071")

team_codes <- read.table("D:/Users/Jon/Documents/Data Science Projects/March Madness 18/Teams.csv", header=TRUE, sep=",")
full_data <- read.table("D:/Users/Jon/Documents/Data Science Projects/March Madness 18/RegularSeasonCompactResults.csv", header=TRUE, sep=",")
tourney_data <- read.table("D:/Users/Jon/Documents/Data Science Projects/March Madness 18/TourneyCompactResults.csv", header=TRUE, sep=",")
kenpom <- read.table("D:/Users/Jon/Documents/Data Science Projects/March Madness 18/kenpom.csv", header=TRUE, sep=",")



first <- 145290 # First index of the 2017 season in Reg_season_data
reg_season_data <- full_data[first:150684,]

#prev_row works correctly
#prev_row returns NA if entry is the first agme of season
prev_row <- function(ID1,cur_row){
  searching <- TRUE
  j <- cur_row-1
  while(searching){
    if(j==0){
      searching = FALSE
      return(NA)
    }else if(reg_season_data[j,3]==ID1 || reg_season_data[j,5]==ID1){
      return(j)
      searching=FALSE
    }
    j<-j-1
    
  }
}



#new lists to create to add to the frame
# these lists all contain stats GOING INTO THE GAME
Wgame_num <- c() #game number of Wteam in row 
Lgame_num <- c() #game number of Lteam in row 
Wavg_score <- c() # avg pts scored of winning team before game
Lavg_score <- c() # avg pts scored of losing team before game
Wavg_allowed <- c() # avg pts allowed of losing team before game
Lavg_allowed <- c() # avg pts allowed of losing team before game
Wprev_wins <- c()
Lprev_wins <- c()
Wopp_games <- c()
Lopp_games <- c()
Wopp_wins <- c()
Lopp_wins <- c()



#function will compute the desired data for the new row
#function also does the handling of previous average when that doesn't make sense.
#function requires the above named lists to be defined and populate up to the current row
create_new_data <- function(cur_row){
  win_prev <- prev_row(reg_season_data[cur_row,3],cur_row) #previous row of game for winning team
  lose_prev <- prev_row(reg_season_data[cur_row,5],cur_row) #previous row of game for losing team
  # the following if-else's are necessary to do the calculations with the correct number
  # in the previous row
  if(is.na(win_prev)){
    Wnew_avg <- 0
    Wi <- 1
    Wnew_allow <- 0
    Wnew_prev_wins <-0
  }else if(reg_season_data[cur_row,3]==reg_season_data[win_prev,3]){ #win team won last
    Wi <- Wgame_num[win_prev]+1 #number of games previously palyed by winning team
    Wnew_prev_wins <- Wprev_wins[win_prev]+1
    Wnew_avg <- ( Wavg_score[win_prev]*(Wi-2)  + reg_season_data[win_prev, 4])/(Wi-1)
    Wnew_allow <- ((Wi-2)*Wavg_allowed[win_prev]+reg_season_data[win_prev, 6])/(Wi-1)
  } else { #Wteam lost last
    Wi <- Lgame_num[win_prev]+1
    Wnew_prev_wins <- Lprev_wins[win_prev]
    Wnew_avg <- (Lavg_score[win_prev]*(Wi-2)+reg_season_data[win_prev,6])/(Wi-1)
    Wnew_allow <-((Wi-2)*Lavg_allowed[win_prev]+reg_season_data[win_prev, 4])/(Wi-1)
  }
  if(is.na(lose_prev)){
    Lnew_avg <- 0
    Li <- 1
    Lnew_allow <- 0
    Lnew_prev_wins <- 0
  } else if(reg_season_data[cur_row,5]==reg_season_data[lose_prev,3]){ #Lteam won prev
    Li <- Wgame_num[lose_prev]+1
    Lnew_prev_wins <- Wprev_wins[lose_prev]+1
    Lnew_avg <- (Wavg_score[lose_prev]*(Li-2)+reg_season_data[lose_prev,4])/(Li-1)
    Lnew_allow <- (Wavg_allowed[lose_prev]*(Li-2)+reg_season_data[lose_prev, 6])/(Li-1)
  } else{ #L team lost prev
    Li <- Lgame_num[lose_prev]+1
    Lnew_prev_wins <- Lprev_wins[lose_prev]
    Lnew_avg <- (Lavg_score[lose_prev]*(Li-2) + reg_season_data[lose_prev,6])/(Li-1)
    Lnew_allow <- (Lavg_allowed[lose_prev]*(Li-2)+reg_season_data[lose_prev, 4])/(Li-1)
  }
  temp <- c(Wnew_avg,Lnew_avg,Wi,Li, Wnew_allow,Lnew_allow,Wnew_prev_wins,Lnew_prev_wins)
  return(temp)
}

len <- length(reg_season_data[,1])
ptm <- proc.time()
for(i in 1:len){
  temp <- create_new_data(i)
  Wavg_score <- c(Wavg_score,temp[1])
  Lavg_score <- c(Lavg_score, temp[2])
  Wgame_num <- c(Wgame_num,temp[3])
  Lgame_num <- c(Lgame_num,temp[4])
  Wavg_allowed <- c(Wavg_allowed,temp[5])
  Lavg_allowed <- c(Lavg_allowed,temp[6])
  Wprev_wins <- c(Wprev_wins,temp[7])
  Lprev_wins <- c(Lprev_wins,temp[8])
}
proc.time()-ptm

full_data_2017 <- reg_season_data
full_data_2017$Wgame_num <- Wgame_num
full_data_2017$Lgame_num <- Lgame_num
full_data_2017$Wavg_score <- Wavg_score
full_data_2017$Wavg_allowed <- Wavg_allowed
full_data_2017$Lavg_score <- Lavg_score
full_data_2017$Lavg_allowed <- Lavg_allowed
full_data_2017$Wprev_wins <- Wprev_wins
full_data_2017$Lprev_wins <- Lprev_wins

write.csv(full_data_2017, file="full_data_2017_final.csv", row.names=FALSE)




