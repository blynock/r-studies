library(readr)
facebook.raw <- read_csv("C:/Users/Benji/OneDrive/Documents/Warwick/ST404/Assignment 3/R Project/data/facebook.csv")
str(facebook.raw)

facebook.proc <- facebook.raw
facebook.proc$Type <- relevel(as.factor(facebook.proc$Type), ref="Photo")
facebook.proc$Weekend <- as.factor(facebook.proc$Weekend)
facebook.proc$Paid <- as.factor(facebook.proc$Paid)
facebook.proc$Category <- as.factor(facebook.proc$Category)
#facebook.proc$Month <- as.factor(facebook.proc$Month)

library(dplyr)
#tidy up a bit

# rename Postlikes column to have consistent capitalisation with PageLikes
facebook.proc <- rename(facebook.proc, PostLikes = Postlikes)

# adding season category variable, might be worth analysing
# facebook.proc$Season <- 0
# facebook.proc$Season <- ifelse(facebook.proc$Month %in% c(12,1,2), "Winter", facebook.proc$Season)
# facebook.proc$Season <- ifelse(facebook.proc$Month %in% c(3,4,5), "Spring", facebook.proc$Season)
# facebook.proc$Season <- ifelse(facebook.proc$Month %in% c(6,7,8), "Summer", facebook.proc$Season)
# facebook.proc$Season <- ifelse(facebook.proc$Month %in% c(9,10,11), "Autumn", facebook.proc$Season)
# facebook.proc$Season <- as.factor(facebook.proc$Season)

# Centre the page likes?
# facebook.proc$PageLikes_Centred <- facebook.proc$PageLikes - mean(facebook.proc$PageLikes)

summary(facebook.proc)

