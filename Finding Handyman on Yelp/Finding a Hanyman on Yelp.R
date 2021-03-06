########################################
#Selecting a Handyman

#Importing data from Github
library(RCurl)
library(foreign)

url <- "https://docs.google.com/spreadsheets/d/1kkBgMg3Q5-Nmm-lgNmDSZpwIK520g1ggE_8v9uvuUG8/pub?output=csv"
handy <- getURL(url)                
handy <- read.csv(textConnection(handy), header = TRUE)

#Based on Calls eliminae these people from further analysis.
handy <- cbind(handy, notes = "Uncalled")
handy$notes <- as.character(handy$notes)

handy$notes[handy$Name == "Your Local Handyman, Inc" | 
              handy$Name =="Webbâs Handyman Service" | 
              handy$Name =="Victor White Handyman" | 
              handy$Name =="Professional Handyman" |
              handy$Name == "Honey-Do Handyman" |
              handy$Name == "15th Street Handy Man"] <- "Messaged"

handy$notes[handy$Name == "M.Y. Handyman Services" | 
              handy$Name == "Alâs Handyman Service" |
              handy$Name == "Guys OC Handyman" |
              handy$Name == "LC Handyman"] <- "No SFS"

#Splitting list by the notes variables
handy.list <- split(handy, handy$notes)

uncalled <- handy.list$Uncalled


#Summary of the data
str(uncalled)
summary(uncalled)

#Set Seed
set.seed(2015)

###############################################
#K-Means Clustering
list.within <- NULL

for (i in 1:10) {
  #Clustering Exercise
  km.out = kmeans(scale(uncalled[2:3]), i, nstart = 100)
  
  #Evaluating the clusters based on total within sum of squares
  list.within <- rbind(list.within,km.out$tot.withinss)
  
}

#Plotting Screeplot
plot(list.within,type="b", main = "Sum of Squared Scree Plot" , xlab="Number of Clusters",
     ylab="Within groups sum of squares", col = "red")

num.clusters <- 4

#Performig the optimal clustering based on the plot above.
km.out = kmeans(scale(uncalled[2:3]), num.clusters, nstart = 100)

#Summarizing clusters and visualizing in two dimentional graph
km.out
plot(uncalled[2:3], col = (km.out$cluster + 1), main = "K-Means Clustering of Handymen", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
#Appending to original data
uncalled.clustered <- cbind(uncalled, km.out$cluster)


###############################################
#Hierarchical Clustering

hc.complete <- hclust(dist(scale(uncalled[2:3])), method="complete")
hc.average <- hclust(dist(scale(uncalled[2:3])), method="average")
hc.single <- hclust(dist(scale(uncalled[2:3])), method="single")

#Visualizing the clusters
par(mfrow=c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", ylab="")
plot(hc.average, main = "Average Linkage", xlab = "", ylab="")
plot(hc.single, main = "Single Linkage", xlab = "", ylab="")


uncalled.clustered <- cbind(uncalled.clustered, h.complete = cutree(hc.complete, num.clusters))
uncalled.clustered <- cbind(uncalled.clustered, h.average = cutree(hc.average, num.clusters))
uncalled.clustered <- cbind(uncalled.clustered, h.single = cutree(hc.single, num.clusters))


###############################################
#Visualizing Clustering Techniques shows that 3 of these reviews stand out in terms of 
#high reviews and high ratings
par(mfrow=c(1,4))
plot(uncalled.clustered[2:3], col = (km.out$cluster + 1), main = "K-Means Clustering", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
plot(uncalled.clustered[2:3], col = (uncalled.clustered$`cutree(hc.complete, 6)` + 1), main = "Hierarchical Clustering - Complete", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
plot(uncalled.clustered[2:3], col = (uncalled.clustered$`cutree(hc.average, 6)` + 1), main = "Hierarchical Clustering - Average", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)
plot(uncalled.clustered[2:3], col = (uncalled.clustered$`cutree(hc.single, 6)` + 1), main = "Hierarchical Clustering - Single", ylab="Yelp Stars", xlab="Yelp Reviews", pch=20, cex=2)


##################
#Adding questions to ask Handymen
uncalled <- cbind(uncalled, SFS ="",  licensed="", provide.quote.ahead="", work.guaranteed="", work.in.team="")

