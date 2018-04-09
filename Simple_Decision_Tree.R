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
#Before dettaching the full data set we compute the naive "record-predictor" accuracy
naive_counter <- 0
total <-0
for(i in 1:length(Team1_pct)){
  if(Team1_pct[i] >Team2_pct[i] && Winning_team[i]=="1"){
    naive_counter <- naive_counter+1
    total <- total+1
  } else if(Team2_pct[i] > Team1_pct[i] && Winning_team[i] =="2"){
    naive_counter <- naive_counter+1
    total <- total+1
  } else if(Team2_pct[i] != Team1_pct[i]){
    total <- total+1
  }
  #Now suppose we predicted half the remaining games correctly
}
not_counted <- length(Team1_pct)-total
new_right <- floor(not_counted/2)
naive_counter <- naive_counter + new_right
naive_accuracy <- naive_counter/length(Team1_pct) #67% accurate



detach(data)
# Now we want to subset the games for which there exist previous data for both teams
desired <- c()
for(i in 1:5395){
  if(Avg_score1[i]!=0 && Avg_score2[i]!=0 && Avg_allow1[i]!=0 && Avg_allow2[i] !=0){
    desired <- c(desired,i)
  }
}
#This is the subseted data-frame
exclude_first <- Scrambled[desired,]
#only desired columns are now attached
attach(exclude_first)
#First we build a multivariable regression to predict the difference team1 score - team2 score
# No saber metric data used in this model
simple_dif_reg<- lm(dif ~ Avg_score1+Avg_score2+Avg_allow1+Avg_allow2)
predicted_dif <- predict(simple_dif_reg)
residuals <- predicted_dif - dif
# The following plot show first that our model is sufficiently complex because the residuals
# are randomly distributed.
plot(Avg_score1,residuals)
# These plots show how noisy the data is so our R^2 =0.3 is fine
plot(Avg_score1,dif)
plot(Avg_allow2,dif)
plot(Avg_score2,dif)
plot(Avg_allow1,dif)


require("rpart")
require("rattle")

#Predict winning team as 1 or 2

full_tree <- rpart(Winning_team ~predicted_dif+Team1_pct +Team2_pct+Avg_score1+Avg_score2+Avg_allow1+Avg_allow2+home)
fancyRpartPlot(full_tree)
predictions <- predict(full_tree)

#Begin the accuracy counter
tree_counter<-0
for(i in 1:length(predictions[ ,1])){
  if(predictions[i,1]>= predictions[i,2] && Winning_team[i]=="1"){
    tree_counter <- tree_counter+1
  }
  if(predictions[i,1]< predictions[i,2] && Winning_team[i]=="2"){
    tree_counter <- tree_counter+1
  }
}
tree_accuracy <- tree_counter/length(predictions[,1]) #70% accuracy


# Now we will do an SVM Model. Note we cannot use home/away values
svm_model <- svm(Winning_team ~ Team1_pct+Team2_pct+predicted_dif+Avg_allow1+Avg_allow2+Avg_score1+Avg_score2, kernel="linear")
svm_predictions <- predict(svm_model)
svm_counter<-0
for(i in 1:length(svm_predictions)){
  if(svm_predictions[i] == Winning_team[i]){
    svm_counter <- svm_counter+1
  }
}
svm_accuracy <- svm_counter/length(svm_predictions) #68% accuracy
table(svm_predictions,Winning_team) #69% accuracte on the model

#Lastly we want to try to use the home-data with an SVM
home <- as.numeric(home)

svm_home <- svm(Winning_team ~ home+Team1_pct+Team2_pct+predicted_dif+Avg_allow1+Avg_allow2+Avg_score1+Avg_score2, kernel="linear")
svm_home_predictions <- predict(svm_home)
svm_home_counter<-0
for(i in 1:length(svm_home_predictions)){
  if(svm_home_predictions[i] == Winning_team[i]){
    svm_home_counter <- svm_home_counter+1
  }
}
svm_home_accuracy <- svm_home_counter/length(svm_home_predictions) #68% accuracy
table(svm_home_predictions,Winning_team) #69% accuracte on the model


#FInally we try a random forest model
require("randomForest")
rand_forest <- randomForest(Winning_team ~ home+predicted_dif+Team1_pct+Team2_pct+Avg_allow1+Avg_allow2+Avg_score1+Avg_score2)
rand_forest #69% accuracy

