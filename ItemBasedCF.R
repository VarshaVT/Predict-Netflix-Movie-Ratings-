#Item based collaborative filtering: 2 approaches
library(sqldf)

#############################################################################
#importing the data and subsetting
############################################################################

all_files <- as.character( list.files("D:/Business Analytics/Fall 2015/Subset"))
directory <- ("D:/Business Analytics/Fall 2015/Subset/")
fp <- paste(directory, all_files, sep="")
users <- read.csv("D:/Business Analytics/Fall 2015/Subset/users.txt")
users <- users[,2]

freq.mov <- c("1905","5317","6287","11283","12470","14313","15124","15205","16242","16377")

#creating subset dataframe containing top 10 movies and ~ top 500 users
#this will take a crazy amount of time
#much easier if you load the dataset "t.movies" in "Subset" using the line#33

"t.movies <- data.frame()
pb <- txtProgressBar(min = 0, max = 100, style = 3)
for(i in 1:10){
  tmp <- read.csv(fp[i], skip = 1)
  tmp$MovID <- freq.mov[i]
  colnames(tmp) <- c('CustID','Rating','Date','MovID')
  for(j in 1:489){
    t <- tmp[tmp$CustID==users[j],]
    t.movies <- rbind(t.movies, t)
  }
  setTxtProgressBar(pb, round((i/10)*100, 1))
}"

t.movies <- read.csv("D:/Business Analytics/Fall 2015/Subset/t.txt")

#############################################################################
#calculating similarity indices
#############################################################################

#function for calculating the cosine similarity
cosim <- function(u, v){
  return(diag(u%*%v)/sqrt(sum(diag(u%*%t(u)))*sum(diag(v%*%t(v)))))
}


#calculate the similarity matrix in df: simat
simat <- data.frame()

pb <- txtProgressBar(min = 0, max = 100, style = 3)
n <- 10

users <- as.data.frame(users)
colnames(users) <- c("CustID")

#simat using pearson correlation coeff (this will take some time, 
#hence the progress bar)

for(i in 1:(n-1)){
  for(j in (i+1):n){
    m1 <- read.csv(fp[i], skip = 1)
    m2 <- read.csv(fp[j], skip = 1)
    colnames(m1) <- c("CustID", "R1", "Date")
    colnames(m2) <- c("CustID", "R2", "Date")
    m1 <- sqldf('select m1.CustID, m1.R1
                from m1
                inner join users on m1.CustID = users.CustID')
    m2 <- sqldf('select m2.CustID, m2.R2
                from m2
                inner join users on m2.CustID = users.CustID')
    v <- sqldf('select R1, R2
               from m1
               inner join m2 on m1.CustID = m2.CustID')
    #x <- cosim(v[,1], v[,2])
    x <- cor(v[,1], v[,2])
    
    a <- as.integer(freq.mov[i])
    b <- as.integer(freq.mov[j])
    
    simat <- rbind(simat, c(a, b, x))
    simat <- rbind(simat, c(b, a, x))
    rm(m1)
    rm(m2)
    rm(v)
    gc()
    setTxtProgressBar(pb, round((i/(n-1))*100, 0))
  }
}

#naming columns and removing some duplicate rows
colnames(simat) <- c("M1", "M2", "SimInd")
simat <- simat[!(duplicated(simat)),]
simat <- na.omit(simat)
simat <- simat[!(simat$M1==simat$M2),]

head(simat)

#calculate the similarity matrix in df: cosimat
cosimat <- data.frame()

pb <- txtProgressBar(min = 0, max = 100, style = 3)
n <- 10

#cosimat using cosine similarity (again, this will take time hence the pb)
for(i in 1:(n-1)){
  for(j in (i+1):n){
    m1 <- read.csv(fp[i], skip = 1)
    m2 <- read.csv(fp[j], skip = 1)
    colnames(m1) <- c("CustID", "R1", "Date")
    colnames(m2) <- c("CustID", "R2", "Date")
    m1 <- sqldf('select m1.CustID, m1.R1
                from m1
                inner join users on m1.CustID = users.CustID')
    m2 <- sqldf('select m2.CustID, m2.R2
                from m2
                inner join users on m2.CustID = users.CustID')
    v <- sqldf('select R1, R2
               from m1
               inner join m2 on m1.CustID = m2.CustID')
    x <- cosim(v[,1], v[,2])
    
    a <- as.integer(freq.mov[i])
    b <- as.integer(freq.mov[j])
    
    cosimat <- rbind(cosimat, c(a, b, x))
    cosimat <- rbind(cosimat, c(b, a, x))
    rm(m1)
    rm(m2)
    rm(v)
    gc()
    setTxtProgressBar(pb, round((i/(n-1))*100, 0))
  }
}

#naming columns and removing some duplicate rows
colnames(cosimat) <- c("M1", "M2", "CoSimInd")
cosimat <- cosimat[!(duplicated(cosimat)),]
cosimat <- na.omit(cosimat)
cosimat <- cosimat[!(cosimat$M1==simat$M2),]

head(cosimat)

rm(all_files)
rm(directory)
rm(fp)
rm(i, j, n, pb)
rm(a, b, x)

###########################################################################
#predicting using the calculated similarity indices
###########################################################################

#weighted prediction using cosine similarity

#function for predicting movie rating using similarity weighted average
predIBCF <- function(mov, user){
  urat <- t.movies[t.movies$CustID==user,]
  mrat <- cosimat[cosimat$M1==mov,c(2,3)]
  umrat <- sqldf('select CoSimInd, Rating
                 from urat
                 inner join mrat on urat.MovID = mrat.M2')
  umrat <- na.omit(umrat)
  return((umrat$CoSimInd%*%umrat$Rating)/sum(umrat$CoSimInd))
}

cos.pred <- t.movies[sample(1:nrow(t.movies), 500, replace=FALSE),]

rownames(cos.pred) <- seq(length=nrow(cos.pred)) 

#running the predIBCF function for all the elements of cos.pred dataset
for(i in 1:500){
  cos.pred$PredRat[i] <- predIBCF(cos.pred$MovID[i],cos.pred$CustID[i])  
}

head(cos.pred)

#calculating RMSE
sqrt(sum((cos.pred$Rating-cos.pred$PredRat)^2)/nrow(cos.pred))

#writing to file
write.csv(cos.pred, file = "D:/Business Analytics/Fall 2015/Businss Intelligence Software & Techniques/Group Project/cosinepred.txt")


#prediction using kNN with k = 5 and correlation similarity

#function for predicting movie rating using kNN average
predIBCFnn <- function(mov, user){
  urat <- t.movies[t.movies$CustID==user,]
  mrat <- simat[simat$M1==mov,c(2,3)]
  umrat <- sqldf('select SimInd, Rating
                 from urat
                 inner join mrat on urat.MovID = mrat.M2')
  umrat <- na.omit(umrat)
  umrat <- umrat[order(umrat$SimInd),]
  umrat <- tail(umrat,5)
  return(mean(umrat$Rating))
}

knn.pred <- t.movies[sample(1:nrow(t.movies), 500, replace=FALSE),]

rownames(knn.pred) <- seq(length=nrow(cos.pred)) 


for(i in 1:500){
  knn.pred$PredRat[i] <- predIBCFnn(knn.pred$MovID[i],knn.pred$CustID[i])  
}

head(knn.pred)

#calculating RMSE
sqrt(sum((knn.pred$Rating-knn.pred$PredRat)^2)/nrow(knn.pred))

#writing to file
write.csv(cos.pred, file = "D:/Business Analytics/Fall 2015/Businss Intelligence Software & Techniques/Group Project/knnpred.txt")
