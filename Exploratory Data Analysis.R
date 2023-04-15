#################################################
##                 PROG8430                    ##
##               ASSIGNMENT 01                 ## 
#################################################
#################################################
##          Written by Kishan Patel            ## 
##               ID: 8781642                   ##
#################################################
##      Exploratory Data Analysis with 'R'     ##
#################################################

#For clearing existing plots
if(!is.null(dev.list())) dev.off()

#For clearing the console
cat("\014")

#For clearing workspace
rm(list=ls())

setwd("C:/Users/Admin1/Documents/R")

options(scipen = 9)

load("PROG8430_Assign_Explore_KP.Rdata")

str(PROG8430_Assign_Explore)

head(PROG8430_Assign_Explore)


#Answer 1.1.a
m.status_kp<-aggregate(PROG8430_Assign_Explore[10],by=list(PROG8430_Assign_Explore$m.status),FUN = sum,na.rm=TRUE)
m.status_kp

#Answer 1.1.b
max_kp<-which.max(scr_kp)
scr_kp[max_kp]

#Answer 1.2.a
asia_kp<-PROG8430_Assign_Explore[PROG8430_Assign_Explore$nation=="Asia",c("nation","age","n.child")]
round(mean(asia_kp$age),digits=2)

#Answer 1.2.b
round(weighted.mean(asia_kp$age,w=asia_kp$n.child),digits = 2)

#Answer 1.3.a
gender_kp<-aggregate(PROG8430_Assign_Explore$score,by=list(PROG8430_Assign_Explore$gender),FUN= mean,na.rm=TRUE)

#Answer 1.3.b
m_kp<-max(gender_kp$x)
gender_kp[gender_kp$x==m_kp,]

#Answer 1.4
quantile(PROG8430_Assign_Explore$time1,c(.34,.63))

#Answer 2.1.a
pol_kp<-table(PROG8430_Assign_Explore$political)
pie(pol_kp,main="number of respondents by political affiliation")

#Answer 2.1.b
pol_kp[which.max(pol_kp)]

#Answer 2.1.c
pol_kp[which.min(pol_kp)]

#Answer 2.2.a
reg_kp<-PROG8430_Assign_Explore[PROG8430_Assign_Explore$group=='treat',]
reg_kp<-prop.table(table(reg_kp$nation))
reg_kp

#Answer 2.2.b
reg_kp[which.max(reg_kp)]

#Answer 2.2.c
reg_kp[which.min(reg_kp)]

#Answer 2.3.a
scr_kp<-tapply(PROG8430_Assign_Explore$scr,PROG8430_Assign_Explore$nation,mean)
barplot(scr_kp,ylim=c(0,1),main="Standardized test scores",ylab="Region")

#Answer 2.3.b
min_kp<-which.min(scr_kp)
scr_kp[min_kp]

#Answer 2.3.c
max_kp<-which.max(scr_kp)
scr_kp[max_kp]

#Answer 2.4.a
hist(PROG8430_Assign_Explore$food,breaks = 5,ylim = c(0,1500),main="percentage of household income gone for food",xlab = "Food", ylab = "Income")

#Answer 2.5.a
boxplot(income~m.status,data=PROG8430_Assign_Explore,main="box plot",xlab = "Marital status", ylab = "Income",pch=20)

#Answer 2.5.d
sd_kp<-aggregate(PROG8430_Assign_Explore$income,by=list(PROG8430_Assign_Explore$m.status),FUN=sd,na.rm=TRUE)
smax_kp<-max(sd_kp$x)
sd_kp[sd_kp$x==smax_kp,]

#Answer 2.6.a
hist(PROG8430_Assign_Explore$income,breaks = 10,xlab = "income",main="histogram for income")

#Answer 2.6.b
hist(PROG8430_Assign_Explore$scr,breaks=13,ylim=c(0,600),xlim=c(-2,3), xlab = "Standardized Score",main="Standardized Score Histogram")

#Answer 2.6.c
plot(income~scr,data=PROG8430_Assign_Explore,col=2,main="scatter plot", xlab= "Income", ylab= "Standardized Score")

#Answer 2.6.e
cor.test(PROG8430_Assign_Explore$income,PROG8430_Assign_Explore$scr,method = "spearman",exact = FALSE)

#Answer 3.1.a
qqnorm(PROG8430_Assign_Explore$score,main="Political Awareness Test")

#Answer 3.1.b
qqline(PROG8430_Assign_Explore$score)
shapiro.test(PROG8430_Assign_Explore$score)

#Answer 3.2.a
test<-t.test(score~group,data=PROG8430_Assign_Explore)
test

#Answer 3.2.c
boxplot(score~group,data=PROG8430_Assign_Explore,main="box plot",xlab = "group",pch=20)

#Answer 3.3.a
boxplot(score~nation,data=PROG8430_Assign_Explore,main="box plot",xlab = "nation",pch=20)

#Answer 3.3.b
summary(aov(Pol~political,data=PROG8430_Assign_Explore))
boxplot(Pol~political,data=PROG8430_Assign_Explore,main="box plot",xlab = "political afiliation", ylab = "Political Involvement",pch=20)




