
movies <- read.csv("C:\\Users\\Varsha\\OneDrive\\BI Project\\Netflix\\movies\\ratings.csv", header=TRUE, sep = ",")
colnames(netflix) <- c("CustID", "Rating", "Date", "MovID")
avgMov <- as.data.frame(aggregate(x=netflix$Rating, by=list(netflix$MovID), FUN="mean"))
avgCust <- as.data.frame(aggregate(x=netflix$Rating, by=list(netflix$CustID), FUN="mean"))
colnames(avgMov) <- c("MovID", "MovAvg")
colnames(avgCust) <- c("CustID", "CustAvg")

head(movies)

#sampling
s.movies <- netflix[sample(1:nrow(netflix), 5000000, replace=FALSE),]

#merging
s.movies <- merge(x = s.movies, y = avgMov, by = "MovID", all = TRUE)
s.movies <- na.omit(s.movies)
s.movies <- merge(x = s.movies, y = avgCust, by = "CustID", all = TRUE)
s.movies <- na.omit(s.movies)

#method 1: with simple averages
lm.1 <- lm(Rating ~ CustAvg + MovAvg, data=s.movies)
summary(lm.1)

#method 2: interaction effect
lm.2 <- lm(Rating ~ CustAvg + MovAvg + (CustAvg*MovAvg), data=s.movies)
summary(lm.2)

#method 3: transformed variables
hist(s.movies$CustAvg)
hist(s.movies$MovAvg)

library(car)

c.lambda <- powerTransform(s.movies$CustAvg)
m.lambda <- powerTransform(s.movies$MovAvg)
s.movies$tCAvg <- bcPower(s.movies$CustAvg, 1.502679)
s.movies$tMAvg <- bcPower(s.movies$MovAvg, 1.637408)



hist(s.movies$tCAvg)
hist(s.movies$tMAvg)

lm.3 <- lm(Rating ~ tCAvg + tMAvg, data=s.movies)
summary(lm.3)

#method 4: transformation with interaction
lm.4 <- lm(Rating ~ tCAvg + tMAvg + (tCAvg*tMAvg), data=s.movies)
summary(lm.4)

#merging movie release date
names <- read.csv("file:///D:/Business Analytics/Summer 2015/Predictive Marketing Analytics/SAS Data & Programs/SASUniversityEdition/Netflix/download/movie_titles.txt", header=FALSE, sep = ",")
colnames(names) <- c("MovID", "Release", "Title")

s.movies <- merge(x = s.movies, y = names, by = "MovID", all.x = TRUE)

drops <- c("Title")
s.movies <- s.movies[, !(names(s.movies) %in% drops)]
s.movies$Date <- as.Date(s.movies$Date)

#count <- c()
#
#total <- 17770
#pb <- txtProgressBar(min = 0, max = 100, style = 3)
#
#for(i in 1:17770){
#  count[i] <- nrow(subset(movies, movies$MovID == i))  
#  setTxtProgressBar(pb, round((i/total)*100, 2))
#}

#count the ratings per movie
library(plyr)

t.movies <- movies
t.movies$MovID <- as.factor(t.movies$MovID)

?count
class(t.movies$MovID)
movct <- c()
movct <- count(t.movies, c('MovID'))

s.movies <- merge(x = s.movies, y = movct, by = "MovID", all = TRUE)
s.movies <- na.omit(s.movies)

rm(t.movies)
rm(movct)

#method 5: with simple averages and movie count
rm(lm.1)
rm(lm.2)
rm(lm.3)
rm(lm.4)
rm(c.lambda)
rm(m.lambda)

lm.5 <- lm(Rating ~ CustAvg + MovAvg + freq, data=s.movies)
summary(lm.5)

rm(lm.5)

#method 6: transformed variables, interaction effects and moviecount
lm.6 <- lm(Rating ~ tCAvg + tMAvg + (tCAvg*tMAvg) + freq, data=s.movies)

summary(lm.6)

rm(lm.6)

#method 7: transformed variables, interaction effects also with freq
lm.7 <- lm(Rating ~ tCAvg + tMAvg + (tCAvg*tMAvg) + freq + (freq*tCAvg), data=s.movies)
summary(lm.7)

hist(resid(lm.7))

rm(lm.7)

rm(avgCust)
rm(avgMov)
rm(movies)
rm(names)

#method 8: date of rating - release date
s.movies$when <- as.numeric(format(s.movies$Date, "%Y")) - as.numeric(levels(s.movies$Release))[s.movies$Release]

s.movies <- s.movies[!(s.movies$when < 0), ]
s.movies <- na.omit(s.movies)

hist(s.movies$when)
min(s.movies$when)

summary(powerTransform(s.movies$when))

lm.8 <- lm(Rating ~ tCAvg + tMAvg + (tCAvg*tMAvg) + freq + (freq*tCAvg) + twhen, data=s.movies)


################################################################
##Expample data
user <- c(1, 1, 2, 2, 3, 3, 3, 4, 4)
movie <- c(1, 3, 1, 2, 1, 2, 3, 2, 3)
rating <- c(2, 3, 5, 2, 3, 3, 1, 2, 2)


x <- cbind(user, movie, rating)
x <- data.frame(x)

library(recommenderlab)
library(reshape2)
library(ggplot2)


library(Matrix)
UIMatrix <- sparseMatrix(i = x$user,
                         j = x$movie,
                         x = x$rating)

library(recommenderlab)

R <- as.matrix(UIMatrix)

r <- as(R, "realRatingMatrix")
r_m <- normalize(r)

image(r, main = "Raw Ratings")       
image(r_m, main = "Normalized Ratings")

##rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score", method="Jaccard",minRating=1))

rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))

recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom

print(rec)
names(getModel(rec))
getModel(rec)$nn


# Convert prediction into list, user-wise
as(recom, "list")

as(r, "matrix")
as(recom, "matrix") # Is full of ratings. NAs disappear


library(xlsx)
write.table(cos.pred,"C:\\Users\\Varsha\\OneDrive\\BI Project")
