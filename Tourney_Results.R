Scores <- read.table("D:/Users/Jon/Documents/Data Science Projects/March Madness 18/TourneyCompactResults.csv", header=TRUE, sep=",")
Seeds <- read.table("D:/Users/Jon/Documents/Data Science Projects/March Madness 18/TourneySeeds.csv", header=TRUE, sep=",")

#We first need to transform the data into a more usable format
len_seeds <- length(Seeds[,1])
New_Seeds <- c()
for(i in 1:len_seeds){
  temp <- Seeds[i,2]
  temp <-as.numeric(substr(temp,2,3))
  New_Seeds <- c(New_Seeds,temp)
}

attach(Scores)
#The following loop is done in a not efficient manner because the length of the data 
# is short enough that efficiency was not a key concern
len_scores <- length(Scores[ ,1])
counter <- 0
for(i in 1:len_scores){
  Cur_seas <- Scores[1,i]
  #Find seeds of both teams
  for(j in 1:len_seeds){
    if(Seeds[j,3]==Wteam[i]){
      Wseed <- New_Seeds[j]
    }
    if(Seeds[j,3]==Lteam[i]){
      Lseed <- New_Seeds[j]
    }
  }
  if(Wseed < Lseed){
    counter <- counter+1
  }
  if(Wseed == Lseed){
    counter <- counter+0.5
  }
}
accuracy <- counter/len_scores #66% accuracy