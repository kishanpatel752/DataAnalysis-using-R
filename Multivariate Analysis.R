#################################################
##                 PROG8430                    ##
##               ASSIGNMENT 04                 ## 
#################################################
#################################################
##          Written by Kishan Patel            ## 
##               ID: 8781642                   ##
#################################################
##       Multivariate Linear Regression        ##
#################################################

#For clearing existing plots
if(!is.null(dev.list())) dev.off()

#For clearing the console
cat("\014")

#For clearing workspace
rm(list=ls())

setwd("C:/Users/Admin1/Documents/R")

##################################################
### Installing Libraries                        ##
##################################################

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(lattice)){install.packages("corrgram")}
library("corrgram")

##############################
## Read in Data             ##
##############################
options(scipen = 9)

load("PROG8430_Assign_MLR_21F.Rdata")

MLR_KP <- PROG8430_Assign_MLR_21F

head(MLR_KP)

##############################
## Reducing Number of Rows  ##
##############################

summary(MLR_KP)

#Answer 1.1
#Look like time2 has multiple NAs!

MLR_KP <- MLR_KP[-c(17)]

head(MLR_KP,8)


#Answer 1.2
# Identify Low Variance
#age has the lowest coef. var
stat.desc(MLR_KP) 
summary(MLR_KP)

table(MLR_KP$age)

MLR_KP <- MLR_KP[-c(6)]

head(MLR_KP,7)


#Answer 1.3
#Identify High Correlation

cor(MLR_KP[c(8,9,10,11,12,13,14,15,16,17)],method="spearman")

#dropping time1 since it seems highly correlated
MLR_KP <- MLR_KP[-c(15)]

head(MLR_KP)

names(MLR_KP) <- c("id_KP", "group_KP", "hs.grad_KP","nation_KP","gender_KP","m.status_KP",
                    "political_KP","n.child_KP","income_KP","food_KP","housing_KP","other_KP","score_KP","pol_KP","time3_KP",
                    "scr_KP" )

summary(MLR_KP)

##########################################################################################################################

#Answer 2

group_dummies_KP <- model.matrix(~group_KP -1, data = MLR_KP)
head(group_dummies_KP)

hsgrad_dummies_KP <- model.matrix(~hs.grad_KP -1,data=MLR_KP)
head(hsgrad_dummies_KP)

nation_dummies_KP <- model.matrix(~nation_KP -1,data=MLR_KP)
head(nation_dummies_KP)

gender_dummies_KP <- model.matrix(~gender_KP -1,data=MLR_KP)
head(gender_dummies_KP)

m.status_dummies_KP <- model.matrix(~m.status_KP -1,data=MLR_KP)
head(m.status_dummies_KP)

political_dummies_KP <- model.matrix(~political_KP -1,data=MLR_KP)
head(political_dummies_KP)

MLR_dummy_KP <-  cbind(MLR_KP, group_dummies_KP, hsgrad_dummies_KP,nation_dummies_KP,
                        gender_dummies_KP,m.status_dummies_KP,political_dummies_KP)
head(MLR_dummy_KP)

MLR_dummy_KP <- MLR_dummy_KP[-c(2,3,4,5,6,7)]
head(MLR_dummy_KP)

##########################################################################################################################

#Answer 3.1

boxplot(MLR_KP$n.child_KP, main = "Boxplot for Number of Child") #Outlier

boxplot(MLR_KP$income_KP, main = "Boxplot for Annual Houshold Income")

boxplot(MLR_KP$food_KP, main = "Boxplot for Percentage spent on food")

boxplot(MLR_KP$housing_KP, main = "Boxplot for Percentage spent on housing") #Outlier

boxplot(MLR_KP$other_KP, main = "Boxplot for Percentage spent on Other Expenses") #Outlier

boxplot(MLR_KP$score_KP, main = "Boxplot for Score on Political Awareness Test") #Outlier

boxplot(MLR_KP$pol_KP, main = "Boxplot of Measure for Political Involvement")

boxplot(MLR_KP$time3_KP, main = "Boxplot for Time taken on section 2")

boxplot(MLR_KP$scr_KP, main = "Boxplot for Standardized Score Test") #Outlier

#Answer 3.2
#Answer is in word file
##########################################################################################################################

##########################################################################################################################
#Answer 4.1

corrgram(MLR_dummy_KP, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Political Engagement")

correlation_KP <- cor(MLR_dummy_KP, method = "spearman")
round(correlation_KP,4)

##########################################################################################################################

#Answer 5.1
SLR_Mod1_KP <- lm(pol_KP~score_KP, data= MLR_KP)
plot(pol_KP~score_KP, data= MLR_KP,
     main= 'Simple Linear Regression Model 1 (Pol vs Score)',
     xlab= 'Score on Political Awareness Test',
     ylab= 'Measurement of Political Involvement',
     pch=20, cex=0.80)
abline(SLR_Mod1_KP)


#Answer 5.2
SLR_Mod2_KP <- lm(pol_KP~scr_KP, data= MLR_KP)
plot(pol_KP~scr_KP, data= MLR_KP,
     main= 'Simple Linear Regression Model 2 (Pol vs Scr)',
     xlab= 'Standardized Score Test',
     ylab= 'Measurement of Political Involvement',
     pch=20, cex=0.80)
abline(SLR_Mod2_KP)
        
#Answer 5.3
summary(SLR_Mod1_KP) 
summary(SLR_Mod2_KP)

#Looking at all the values stated in summary (F-statistic p-value, Adjusted R-squared, Residuals, t-stat values), It is very obvious that Model 1 (Pol vs Score) is superior since it has lower P-value, moreover the coefficient of score is contributing significantly towards prediction.

##########################################################################################################################

#Answer 6
#Baseline Model

correlation_KP <- as.data.frame(correlation_KP)
MLR_lm_KP <- lm(correlation_KP$pol_KP ~ correlation_KP$n.child_KP + correlation_KP$income_KP + 
                        correlation_KP$food_KP + correlation_KP$housing_KP + correlation_KP$other_KP + correlation_KP$score_KP + correlation_KP$time3_KP + correlation_KP$scr_KP + correlation_KP$group_KPcontrol + correlation_KP$hs.grad_KPno + correlation_KP$nation_KPAsia
                + correlation_KP$nation_KPEurope + correlation_KP$`nation_KPNorth America` + correlation_KP$gender_KPfemale + correlation_KP$gender_KPmale + correlation_KP$m.status_KPdivorced + correlation_KP$m.status_KPmarried + correlation_KP$m.status_KPnever + correlation_KP$political_KPConservative + correlation_KP$political_KPLiberal + correlation_KP$political_KPNew_Democrat
                , data = correlation_KP, na.action = na.omit )
summary(MLR_lm_KP)


#Backward Model

Bck_MLR_lm_KP <- step(MLR_lm_KP, direction = "backward", details = TRUE)
summary(Bck_MLR_lm_KP)


#Forward Model

min_model_KP <- lm(correlation_KP$pol_KP ~ 1, data = correlation_KP, na.action = na.omit)
Fwd_MLR_lm_KP <- step(min_model_KP, direction = "forward", scope = (~ correlation_KP$n.child_KP + correlation_KP$income_KP + 
                                                                correlation_KP$food_KP + correlation_KP$housing_KP + correlation_KP$other_KP + correlation_KP$score_KP + correlation_KP$time3_KP + correlation_KP$scr_KP + correlation_KP$group_KPcontrol + correlation_KP$hs.grad_KPno + correlation_KP$nation_KPAsia
                                                                + correlation_KP$nation_KPEurope + correlation_KP$`nation_KPNorth America` + correlation_KP$gender_KPfemale + correlation_KP$gender_KPmale + correlation_KP$m.status_KPdivorced + correlation_KP$m.status_KPmarried + correlation_KP$m.status_KPnever + correlation_KP$political_KPConservative + correlation_KP$political_KPLiberal + correlation_KP$political_KPNew_Democrat), details = TRUE)
summary(Fwd_MLR_lm_KP)

##########################################################################################################################
#Answer 7
BckMLRFit <- predict(Bck_MLR_lm_KP)
BckMLRRes <- residuals(Bck_MLR_lm_KP)

FwdMLRFit <- predict(Fwd_MLR_lm_KP)
FwdMLRRes <- residuals(Fwd_MLR_lm_KP)

#Numerically
shapiro.test(BckMLRRes)
shapiro.test(FwdMLRRes)

#Graphically
par(mfrow= c(2,2))
plot(Bck_MLR_lm_KP)
par(mfrow= c(1,1))

par(mfrow= c(2,2))
plot(Fwd_MLR_lm_KP)
par(mfrow= c(1,1))
