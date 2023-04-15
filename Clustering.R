#################################################
##                 PROG8430                    ##
##               ASSIGNMENT 03                 ## 
#################################################
#################################################
##          Written by Kishan Patel            ## 
##               ID: 8781642                   ##
#################################################
##                Clustering                   ##
#################################################

#For clearing existing plots
if(!is.null(dev.list())) dev.off()

#For clearing the console
cat("\014")

#For clearing workspace
rm(list=ls())

setwd("C:/Users/Admin1/Documents/R")

options(scipen = 9)

#Installing required Packages
if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

if(!require(cluster)){install.packages("cluster")}
library("cluster")

if(!require(factoextra)){install.packages("factoextra")}
library("factoextra")


load("PROG8430_Clst_21F.Rdata")

str(PROG8430_Clst_21F)

head(PROG8430_Clst_21F)



#Answer 1.1
#Creating a standardize function
norm_KP <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}

#Standardizimg all the variables
PROG8430_Clst_21F$FoodEx_KP <- norm_KP(PROG8430_Clst_21F$Food)  #Standardized Food Variable
PROG8430_Clst_21F$EntrEx_KP <- norm_KP(PROG8430_Clst_21F$Entr)  #Standardized Entertainment Variable
PROG8430_Clst_21F$EducEx_KP <- norm_KP(PROG8430_Clst_21F$Educ)  #Standardized Education Variable
PROG8430_Clst_21F$TranEx_KP <- norm_KP(PROG8430_Clst_21F$Tran)  #Standardized Transportation Variable
PROG8430_Clst_21F$WorkEx_KP <- norm_KP(PROG8430_Clst_21F$Work)  #Standardized Work Variable
PROG8430_Clst_21F$HousEx_KP <- norm_KP(PROG8430_Clst_21F$Hous)  #Standardized Housing Variable
PROG8430_Clst_21F$OthrEx_KP <- norm_KP(PROG8430_Clst_21F$Othr)  #Standardized Other Variable

head(PROG8430_Clst_21F)
summary(PROG8430_Clst_21F)



#Answer 2.1
hist(PROG8430_Clst_21F$Food, main = "Portion of Income Spent by Participants on Food", xlab = "Percentage", ylab = "Participants", xlim =c(0.0,0.25), ylim = c(0,250))
hist(PROG8430_Clst_21F$Entr, main = "Portion of Income Spent by Participants on Entertainment", xlab = "Percentage", ylab = "Participants", xlim =c(0.0,0.15), ylim = c(0,160))
hist(PROG8430_Clst_21F$Educ, main = "Portion of Income Spent by Participants on Education", xlab = "Percentage", ylab = "Participants", xlim =c(0.0,0.3), ylim = c(0,160))
hist(PROG8430_Clst_21F$Tran, main = "Portion of Income Spent by Participants on Transportation", xlab = "Percentage", ylab = "Participants", xlim =c(0.0,0.3), ylim = c(0,160))
hist(PROG8430_Clst_21F$Work, main = "Portion of Income Spent by Participants on Work related stuffs", xlab = "Percentage", ylab = "Participants", xlim =c(0.0,0.12), ylim = c(0,200))
hist(PROG8430_Clst_21F$Hous, main = "Portion of Income Spent by Participants on Housing", xlab = "Percentage", ylab = "Participants", xlim =c(0.25,0.6), ylim = c(0,170))
hist(PROG8430_Clst_21F$Othr, main = "Portion of Income Spent by Participants on Other stuffs", xlab = "Percentage", ylab = "Participants", xlim =c(0.0,0.12), ylim = c(0,200))



#Answer 3.1
clusterData_KP <- PROG8430_Clst_21F[c(8,9)]
str(clusterData_KP)

clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 3, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:3), clusterDoc_KP$centers)

#For KMeans 4
clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 4, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:4), clusterDoc_KP$centers)

#For KMeans 5
clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 5, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:5), clusterDoc_KP$centers)

#For KMeans 6
clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 6, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:6), clusterDoc_KP$centers)

#For KMeans 7
clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 7, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:7), clusterDoc_KP$centers)

#Answer 3.2
#It is in the word file 



#Answer 4.1
#K=4
clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 4, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:4), clusterDoc_KP$centers)

ggplot(data=PROG8430_Clst_21F, aes(x = EntrEx_KP, y = FoodEx_KP, color=cluster)) + geom_point()

ggplot(data=PROG8430_Clst_21F,aes(x = EntrEx_KP, y = FoodEx_KP, color=cluster, shape=cluster)) + geom_point(alpha=.8) +
  geom_point(data=centers,aes(x = EntrEx_KP, y = FoodEx_KP), size=5, stroke=2)

#K-1=3
clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 3, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:3), clusterDoc_KP$centers)

ggplot(data=PROG8430_Clst_21F, aes(x = EntrEx_KP, y = FoodEx_KP, color=cluster)) + geom_point()

ggplot(data=PROG8430_Clst_21F,aes(x = EntrEx_KP, y = FoodEx_KP, color=cluster, shape=cluster)) + geom_point(alpha=.8) +
  geom_point(data=centers,aes(x = EntrEx_KP, y = FoodEx_KP), size=5, stroke=2)

#K+1=5
clusterDoc_KP <- kmeans(clusterData_KP, iter.max=10, centers = 5, nstart = 10)
clusterDoc_KP

PROG8430_Clst_21F$cluster <- factor(clusterDoc_KP$cluster)
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:5), clusterDoc_KP$centers)

ggplot(data=PROG8430_Clst_21F, aes(x = EntrEx_KP, y = FoodEx_KP, color=cluster)) + geom_point()

ggplot(data=PROG8430_Clst_21F,aes(x = EntrEx_KP, y = FoodEx_KP, color=cluster, shape=cluster)) + geom_point(alpha=.8) +
  geom_point(data=centers,aes(x = EntrEx_KP, y = FoodEx_KP), size=5, stroke=2)


#Answer 4.2



#Answer 4.3
PROG8430_Clst_21FSUM_KP <- PROG8430_Clst_21F %>% 
  group_by(cluster) %>% 
  summarise(Food= mean(Food), Entr=mean(Entr), Educ= mean(Educ), Tran=mean(Tran), Work=mean(Work), Hous=mean(Hous), Othr=mean(Othr), N=n())
PROG8430_Clst_21FSUM_KP

#Answer 4.4
#Answer 4.5 both are in word file 