##Load packages
install.packages("dplyr")
library(dplyr)
library(e1071)
install.packages("tidyr")
library(tidyr)

install.packages("recommenderlab")
library(recommenderlab)

###Set the working directory
setwd("C:/Users/mmajid1/Desktop/Recommendation Tools/Data-20190222")

####Read in the data
artists <- read.csv("Artists.dat", sep="\t")
tags <- read.csv("tags.dat", sep="\t")
user_artists<- read.csv("user_artists.dat", sep="\t")
user_taggedartists <- read.csv("user_taggedartists.dat", sep="\t")

##Check unique users and artist IDs
#unique users
#length(unique(user_artists$userID)) #1892
#unique artists/items
#length(unique(user_artists$artistID)) #17632


#summary(user_artists$weight)
#hist(user_artists$weight) #right skewed
#skewness(user_artists$weight)

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
splitdata <- New_user_artists[sample(nrow(New_user_artists), 300), ]

#names(New_user_artists)

#New_user_artists_wide <- spread(New_user_artists, key = artistID, value = trans_weight )

splitdata_wide <- spread(splitdata, key = artistID, value = trans_weight )

#New_user_artists_matrix <- data.matrix(New_user_artists_wide)

splitdata_matrix <- data.matrix(splitdata_wide)


####Computing pearson correlation function

##split the data into train and test

#nrow(New_user_artists_matrix) 

########################################################################################################################################
### Cluster based CF as a function ###
######################################

ClusterBasedCF <- function(splitdata_matrix, N, centers, iter, onlyNew=TRUE){
  
  splitdata_matrix2 <- splitdata_matrix
  
  # fill with average product rating
  colmeans <- colMeans(splitdata_matrix2, na.rm=TRUE)
  
  for (j in colnames(splitdata_matrix2)){
    splitdata_matrix2[, j] <- ifelse(is.na(splitdata_matrix2[ ,j]), colmeans[j], splitdata_matrix2[, j])
  }
  
  km <- kmeans(splitdata_matrix2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(splitdata_matrix, as.data.frame(km$cluster))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction), 
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    if (onlyNew == TRUE){
      unseen <- names(splitdata_matrix[u, is.na(splitdata_matrix[u,])])
      
      prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
  }
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
} 

result<-ClusterBasedCF(splitdata_matrix = splitdata_matrix,centers = 100, iter = 25)









