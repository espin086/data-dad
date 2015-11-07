########################################
#Selecting a Handyman

#Importing data from Github
library(RCurl)
library(foreign)

url <- "https://docs.google.com/spreadsheets/d/1kkBgMg3Q5-Nmm-lgNmDSZpwIK520g1ggE_8v9uvuUG8/pub?output=csv"
handy <- getURL(url)                
handy <- read.csv(textConnection(handy), header = TRUE)

#Summary of the data
str(handy)
summary(handy)

#Set Seed
set.seed(2015)

###############################################
#K-Means Clustering
list.within <- NULL

for (i in 1:10) {
  #Clustering Exercise
  km.out = kmeans(scale(handy[2:3]), i, nstart = 100)
  
  #Evaluating the clusters based on total within sum of squares
  list.within <- rbind(list.within,km.out$tot.withinss)
  
}

#Plotting Screeplot
plot(list.within,type="b", main = "Sum of Squared Scree Plot" , xlab="Number of Clusters",
     ylab="Within groups sum of squares", col = "red")

#Performig the optimal clustering based on the plot above.
km.out = kmeans(scale(handy[2:3]), 6, nstart = 100)

#Summarizing clusters and visualizing in two dimentional graph
km.out
plot(handy[2:3], col = (km.out$cluster + 1), main = "K-Means Clustering of Handymen", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
#Appending to original data
handy.clustered <- cbind(handy, km.out$cluster)


###############################################
#Hierarchical Clustering

hc.complete <- hclust(dist(scale(handy[2:3])), method="complete")
hc.average <- hclust(dist(scale(handy[2:3])), method="average")
hc.single <- hclust(dist(scale(handy[2:3])), method="single")

#Visualizing the clusters
par(mfrow=c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", ylab="")
plot(hc.average, main = "Average Linkage", xlab = "", ylab="")
plot(hc.single, main = "Single Linkage", xlab = "", ylab="")


handy.clustered <- cbind(handy.clustered, cutree(hc.complete, 6))
handy.clustered <- cbind(handy.clustered, cutree(hc.average, 6))
handy.clustered <- cbind(handy.clustered, cutree(hc.single, 6))


###############################################
#Visualizing Clustering Techniques shows that 3 of these reviews stand out in terms of 
#high reviews and high ratings
par(mfrow=c(1,4))
plot(handy.clustered[2:3], col = (km.out$cluster + 1), main = "K-Means Clustering", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
plot(handy.clustered[2:3], col = (handy.clustered$`cutree(hc.complete, 6)` + 1), main = "Hierarchical Clustering - Complete", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
plot(handy.clustered[2:3], col = (handy.clustered$`cutree(hc.average, 6)` + 1), main = "Hierarchical Clustering - Average", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
plot(handy.clustered[2:3], col = (handy.clustered$`cutree(hc.single, 6)` + 1), main = "Hierarchical Clustering - Single", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)


##################
#Selecting Handymen to call
selected <- handy.clustered[which(handy.clustered$`km.out$cluster`==2 & handy.clustered$Stars > 4),]
