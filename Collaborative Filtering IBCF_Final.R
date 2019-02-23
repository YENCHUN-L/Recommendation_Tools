##Load packages
#install.packages("dplyr")
library(dplyr)
library(e1071)
#install.packages("tidyr")
library(tidyr)
install.packages("recommenderlab")
library(recommenderlab)
#install.packages("Matrix")
library(Matrix)
library(proxy)

##############################################Set the working directory
wd = "C:/Users/kiriaannie/Documents/Msc Big Data IESEG/Sem 2/Recommendation System/Group Assignment/Data-20190218"
setwd(wd)
getwd()

##############################################Read in the data

artists <- read.csv("Artists.dat", sep="\t")
tags <- read.csv("tags.dat", sep="\t")
user_artists<- read.csv("user_artists.dat", sep="\t")
user_taggedartists <- read.csv("user_taggedartists.dat", sep="\t")



##Check unique users and artist IDs
#unique users
length(unique(user_artists$userID)) #1892

#unique artists/items
length(unique(user_artists$artistID)) #17632


##Check the distribution of the weights
summary(user_artists$weight)
hist(user_artists$weight) # histogram is right skewed
skewness(user_artists$weight)


##################################################Transform data to fix the skewness using log transformation
New_user_artists <- user_artists
New_user_artists$weight <- as.numeric(New_user_artists$weight)
New_user_artists$trans_weight<-log10(10*New_user_artists$weight) 

##round of the weight values
New_user_artists <- New_user_artists %>% mutate_at(vars(trans_weight), funs(round(., 2)))

hist(New_user_artists$trans_weight)
#str(New_user_artists)
summary(New_user_artists$trans_weight)



#####################################################Convert the dataframe into a wide matrix
##Preprocess data before transforming it into a wide matrix
##Pick only userid,artistid and new transformed weights
New_user_artists <- New_user_artists[,c(1,2,4)]


#for the purpose of fast execution randomly split the dataframe before tranposing only 1000 users were picked

New_user_artists <- New_user_artists[sample(nrow(New_user_artists), 1000), ]

## transform all user id into 4 interger length
New_user_artists$userID<- sprintf("%04d",New_user_artists$userID) 

##add 'u' before all userid numbers eg u0002
New_user_artists$userID <-paste0('u',New_user_artists$userID)

## transform all artist id into 5 interger length
New_user_artists$artistID<- sprintf("%05d",New_user_artists$artistID)

##add 'a' before all artistid numbers eg a00002
New_user_artists$artistID <-paste0('a',New_user_artists$artistID)


############## Use spread function to transpose the data
New_user_artists_wide <- spread(New_user_artists, key = artistID, value = trans_weight )

New_user_artists_wide[is.na(New_user_artists_wide)] <- 0

#Preview the data
New_user_artists_wide[1:10,1:10]


#convert into a matrix
New_user_artists_matrix <- data.matrix(New_user_artists_wide)
row.names(New_user_artists_matrix) <- New_user_artists_matrix[,1]


#drop first column
New_user_artists_matrix<- New_user_artists_matrix[,-1]

#add row names
row.names(New_user_artists_matrix) <- New_user_artists_wide[,1]

New_user_artists_matrix[1:10,1:10]


#######################################Computing pearson correlation function

##split the data into train and test

num_rows <- nrow(New_user_artists_matrix) 

#temp <- as(New_user_artists_matrix,"realRatingMatrix") 

# split into 70/30, takes about 40 mins to run

set.seed(123) # Set a seed to have the same subsets every time 

# Define proportion to be in training set 

p <- 0.7

# Define observations to be in training set

training_locations <- sort(sample(num_rows,floor(p*num_rows)))
train_data <- New_user_artists_matrix[training_locations,]
test_data <- New_user_artists_matrix[-training_locations,]




ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # SimilaritY
 
  similarity_matrix = matrix(, ncol=ncol(test_data), nrow=ncol(train_data), 
                              dimnames = list(colnames(test_data), colnames(train_data)))
 
  
  for (i in 1:ncol(test_data)){
    for (j in 1:ncol(train_data)){     
    r_ui <- test_data[,i]
    r_uj <- train_data[,j]
    r_meanui <- mean(test_data[,i], na.rm=TRUE)
    r_meanuj <- mean(train_data[,j], na.rm=TRUE)
      if (j > i){
        sim <- sum((r_ui-r_meanui)*(r_uj -r_meanuj), na.rm=TRUE)/(sqrt(sum((r_ui-r_meanui)^2)) * sum((r_uj -r_meanuj)^2))
        similarity_matrix[i, j] <- sim
        similarity_matrix[j, i] <- sim
      }
    }
  }
  
  ptm <- proc.time()
  
  
  print("Similarity calculation done")
  # Nearest Neighbor
  NN=10
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
    similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
  }
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  
  train_data[is.na(train_data)] <- 0
  
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
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

############################################################################################
ResultsIBCF <- ItemBasedCF(train_data, test_data, 3, NN=10, onlyNew=TRUE)

prediction <- as.data.frame(ResultsIBCF$prediction)
(-sort(prediction[1,]))[1:20]
TopN2 <- as.data.frame(ResultsIBCF$topN)


