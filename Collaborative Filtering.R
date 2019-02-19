##Load packages
install.packages("dplyr")
library(dplyr)
library(e1071)
install.packages("tidyr")
library(tidyr)

install.packages("recommenderlab")
library(recommenderlab)

###Set the working directory
setwd("C:/Users/kiriaannie/Documents/Msc Big Data IESEG/Sem 2/Recommendation System/Group Assignment/Data-20190218")

####Read in the data
artists <- read.csv("Artists.dat", sep="\t")
tags <- read.csv("tags.dat", sep="\t")
user_artists<- read.csv("user_artists.dat", sep="\t")
user_taggedartists <- read.csv("user_taggedartists.dat", sep="\t")

##Check uniaue users and artist IDs
#unique users
length(unique(user_artists$userID)) #1892
#unique artists/items
length(unique(user_artists$artistID)) #17632


summary(user_artists$weight)
hist(user_artists$weight) #right skewed
skewness(user_artists$weight)

#count <- ungroup(user_artists) %>% 
  #group_by(userID) %>% 
  #summarize(Count=n()) %>% 
  #arrange(desc(Count))

#Mean_weight <- ungroup(user_artists) %>% 
  #group_by(artistID) %>% 
  #summarize(Mean_weight = mean(weight)) %>% 
  #arrange(desc(Mean_weight))

#New_user_artists <- merge(user_artists, count, by.x='userID', by.y='userID', all.x=T)
#New_user_artists <- merge(user_artists, Mean_weight, by.x='userID', by.y='userID', all.x=T)
#New_user_artists$Mean_weight <- as.numeric(New_user_artists$Mean_weight)
#hist(New_user_artists$Mean_weight)
#str(New_user_artists)
#summary(New_user_artists$Mean_weight)

####Transform data to fix the skewness using log transformation
New_user_artists <- user_artists
New_user_artists$weight <- as.numeric(New_user_artists$weight)
New_user_artists$trans_weight<-log10( 10 *New_user_artists$weight) 
hist(New_user_artists$trans_weight)

str(New_user_artists)



summary(New_user_artists$trans_weight)

###Convert the dataframe into a wide matrix
New_user_artists <- New_user_artists[,c(1,2,4)]
names(New_user_artists)
New_user_artists <- spread(New_user_artists, key = artistID, value = trans_weight )
New_user_artists <- data.matrix(New_user_artists)

####Computing pearson correlation function
##split the data into train and test
nrow(New_user_artists) 

train <- New_user_artists[1:1325,]
test <- New_user_artists[1326:1892,]

##1.Using a function

UserBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){

### similarity ###
#Initialize an empty matrix
  similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data), 
                              dimnames = list(rownames(test_data), rownames(train_data)))
  
  for (i in rownames(test_data)){
    for (j in rownames(train_data)){
        sim <- sum((test_data[i,] - rowMeans) * (train_data[j,]-rowmeans), na.rm=TRUE)/(sqrt(sum((test_data[i,]-rowmeans)^2, na.rm=TRUE)) * sqrt(sum((train_data[j,]-rowmeans)^2, na.rm=TRUE)))
        similarity_matrix[i, j] <- sim
    }
  } 
  print("similarity calculation done")
  ### Nearest Neighbors ###
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
  }
  
  print("Nearest Neighbor selection done")
  ### Prediction ###
  # Prepare
  prediction <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  
  TopN <- matrix(, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
  ### Numerator ###
  for (u in rownames(test_data)){
    similarity_vector <- na.omit(similarity_matrix_NN[u, ])
    
    NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]
    
    CM <- colMeans(train_data, na.rm=TRUE)
    for (l in 1:ncol(NN_norm)){
      NN_norm[,l] <- NN_norm[,l] - CM[l]
    }
    NN_norm[is.na(NN_norm)] <- 0
    
    # Numerator
    Num = similarity_vector %*% NN_norm
    
    #Prediction
    prediction[u, ] =  mean(test_data[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
    
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

######Check for results using the  function

ResultsIBCF <- UserBasedCF(train, test, 5, NN= 10, onlyNew=TRUE)

prediction <- as.data.frame(ResultsIBCF$prediction)

TopN <- as.data.frame(ResultsIBCF$topN)




