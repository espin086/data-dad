########################################
#Feeding Babies

#Importing data from Github

library(RCurl)
library(foreign)


url <- "https://raw.githubusercontent.com/espin086/data-dad/master/Optimizing%20the%20Feeding%20of%20Babies/Baby%20Food.csv"
baby.food <- getURL(url)                
baby.food <- read.csv(textConnection(baby.food), header = TRUE, skip =1)

