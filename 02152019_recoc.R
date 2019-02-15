# plot(user_artists$weight)
# p7 <- ggplot(user_artists, aes(x = artistID)) +
#   geom_histogram()
# p7 
# 
# p8 <- ggplot(user_artists, aes(x = userID)) +
#   geom_histogram()
# p8     
# as.character(user_artists$userID)
# as.character(user_artists$artistID)
# spread(user_artists, user_artists$userID, user_artists$weight)
# test<-spread(user_artists, userID, weight)

##################
#https://rstudio-pubs-static.s3.amazonaws.com/275900_f232551965984fffaa35c8ee995d5fad.html
# get product counts
count <- ungroup(user_artists) %>% 
  group_by(artistID) %>% 
  summarize(Count=n()) %>% 
  arrange(desc(Count))

# get mean score for each product
mean_weight <- ungroup(user_artists) %>% 
  group_by(artistID) %>% 
  summarize(Mean_weight = mean(weight)) %>% 
  arrange(desc(Mean_weight))

# merge counts and mean into data frame
data <- merge(user_artists, count, by.x='artistID', by.y='artistID', all.x=T)
data <- merge(user_artists, mean_weight, by.x='artistID', by.y='artistID', all.x=T)

# # drop unneeded columns
# data2 <- data[, c(1:4,7,9:12)]
# 
# # delete rid of stray characters
# data2$UserId <- gsub('#oc-', '', data2$UserId)
# 
# # trim white space
# data2[, c(1:6)] <- lapply(data2[, c(1:6)], trimws)

# # make Score numeric
# data2$Score <- as.numeric(data2$Score)

# create a new data set with a column that groups by product and combines the Summary reviews; this df is used for semantic analysis later
# 
# data3 <- ungroup(data2) %>%
#   group_by(ProductId) %>% 
#   mutate(combine_summary = paste0(Summary, collapse = ' '))

# check lengths
# length(unique(data3$combine_summary))
# length(unique(data3$ProductId))

# end data cleanup on original data; clean data in data3

## for recommenderlab, the data must be imported in a particular format
## the following steps create 'datRlab' in the right format 
# 
# # drop products with fewer than median count
# medianProds <- median(data2$Count)
# 
# datRlab <- ungroup(data3) %>%
#   filter(Count >= medianProds)
# 
# # remove unneded columns
# datRlab <- datRlab[, c(3,1,5)]
# 
# # remove duplicates
# datRlab <- datRlab[!duplicated(datRlab[,c(1,2)]),]



ggplot(user_artists, aes(x=mean_weight)) +
  geom_histogram(binwidth=.01, alpha=.5, position="identity") +
  geom_vline(aes(xintercept=mean(weight)), color="red") +
  annotate("text", x=400, y=5000, label=paste("Mean = ", mean_weight)) +
  labs(x="Mean Score", y="Count",
       title="Distribution of Review Scores") 

  

