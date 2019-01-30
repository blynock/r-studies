# EDA

library(ggplot2)
library(dplyr)
library(ggcorrplot)


str(facebook.proc)

summary(facebook.proc)
# no missing data
# mostly post photos; 90% are photos
# is page likes a "time" variable? it's not monotonic
# ~1/3 are weekend
# min 0, median 19, max 208 - look at hist / density of this


hist(facebook.proc$Month)
# shows fairly evenly spread throughout year which is good
# what to do with month? convert to a factor? seems wrong.
# but assuming that the index of the month is meaningful per se is also wrong
# maybe convert to a seasonal factor (import.r now adds a seasonal factor)

ggplot(facebook.proc, aes(x=Month, y=PageLikes)) + geom_point()
# plot of likes against month shows monotonic but asymptotic relationship

# unconditional distribution of response
ggplot(facebook.proc, aes(x=PostLikes, ..density..)) + geom_histogram() + geom_density()
# looks "kinda" Poisson-like, but clearly overdispersed

mean(facebook.proc$PostLikes)
var(facebook.proc$PostLikes)
# mean - 25.7
# variance - 640.8
# not similar

facebook.proc %>% filter(Paid==0) %>% select("PostLikes")


# Mean-variance relationship
with(facebook.proc,tapply(PostLikes,Category,function(x){
  sprintf("mu = %1.2f, var=%1.2f",mean(x),var(x))}))

with(facebook.proc,tapply(PostLikes,Type,function(x){
  sprintf("mu = %1.2f, var=%1.2f",mean(x),var(x))}))

with(facebook.proc,tapply(PostLikes,Paid,function(x){
  sprintf("mu = %1.2f, var=%1.2f",mean(x),var(x))}))

with(facebook.proc,tapply(PostLikes,Weekend,function(x){
  sprintf("mu = %1.2f, var=%1.2f",mean(x),var(x))}))


mean(facebook.proc[facebook.proc$Paid==0,]$PostLikes)

mean(facebook.proc[facebook.proc$Type=="Link",]$PostLikes)
mean(facebook.proc[facebook.proc$Type=="Status",]$PostLikes)
mean(facebook.proc[facebook.proc$Type=="Photo",]$PostLikes)
mean(facebook.proc[facebook.proc$Type=="Video",]$PostLikes)

mean(facebook.proc[facebook.proc$Category==1,]$PostLikes)
mean(facebook.proc[facebook.proc$Category==2,]$PostLikes)
mean(facebook.proc[facebook.proc$Category==3,]$PostLikes)


cor(facebook.proc$PageLikes, facebook.proc$Month)


