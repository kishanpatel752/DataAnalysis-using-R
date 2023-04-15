#################################################
##                 PROG8430                    ##
##               ASSIGNMENT 05                 ## 
#################################################
#################################################
##          Written by Kishan Patel            ## 
##               ID: 8781642                   ##
#################################################
##               Classification                ##
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

if(!require(pROC)){install.packages("pROC")}
library("pROC")


if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(MASS)){install.packages("MASS")}
library("MASS")

if(!require(klaR)){install.packages("klaR")}
library("klaR")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

##############################
## Read in Data             ##
##############################
options(scipen = 9)

load("Tumor_21F.Rdata")

Tumor_KP <- Tumor_21F

head(Tumor_KP)

##################################
##  Reducing unnecessary Data   ##
##################################

#Answer 1.1
#Identify Low Variance
#Neck has the lowest coef. var
stat.desc(Tumor_KP) 
summary(Tumor_KP)

table(Tumor_KP$Neck)

Tumor_KP <- Tumor_KP[-c(10)]

head(Tumor_KP)

##############################
## Exploratory Analysis     ##
##############################

#Answer 2.1
correlation_KP <- cor(Tumor_KP, method = "spearman")
round(correlation_KP,4)

#We can see from the numbers that Brain and Out is co-linear and Out-Marrow are somewhat co-linear.

#Answer 2.2
Table_Tumor_KP <- table(Tumor_KP$Brain, Tumor_KP$Out, dnn=list("Brain","Out"))
Table_Tumor_KP
prop.table(Table_Tumor_KP, 2) 

#Performing Chi-Squared test to compare the observed value v/s expected value
chisqTumor_KP <- chisq.test(Tumor_KP$Brain, Tumor_KP$Out, correct=FALSE)      
chisqTumor_KP

#Brain and Out variables seem to contribute significantly as they are correlated with each other

chisqTumor_KP$observed   # What we observed
chisqTumor_KP$expected   # If there were no relationship

#Graphical representation
barplot(prop.table(Table_Tumor_KP,2), xlab='Presence of Tumor',ylab='Percentage',main="Brain by Out",
        col=c("darkblue","darkred")
        ,legend=rownames(Table_Tumor_KP), args.legend = list(x = "topleft"))

 
##############################
##    Model Development     ##
##############################

#Answer 3.1

Tumor_glm_KP = glm(Tumor_KP$Brain ~ Tumor_KP$Age + Tumor_KP$Sex + Tumor_KP$Bone + Tumor_KP$Marrow + Tumor_KP$Lung + Tumor_KP$Pleura + Tumor_KP$Liver 
              +Tumor_KP$Skin + Tumor_KP$Supra + Tumor_KP$Axil + Tumor_KP$Media + Tumor_KP$Out,
              family="binomial", data=Tumor_KP, na.action=na.omit)

stp_Tumor_glm_KP <- step(Tumor_glm_KP, direction = "forward")

summary(stp_Tumor_glm_KP)

#Answer 3.2
#User Model 1 (Eliminating Lung variable)
UM1_glm_KP = glm(Tumor_KP$Out ~ Tumor_KP$Age + Tumor_KP$Sex + Tumor_KP$Liver + Tumor_KP$Pleura + Tumor_KP$Marrow
                  +Tumor_KP$Skin + Tumor_KP$Supra + Tumor_KP$Axil + Tumor_KP$Media + Tumor_KP$Brain,
                   family="binomial", data=Tumor_KP, na.action=na.omit)

stp_UM1_glm_KP <- step(UM1_glm_KP, direction = "forward")

summary(stp_UM1_glm_KP)

#User Model 2 (Eliminating Liver Variable)
UM2_glm_KP = glm(Tumor_KP$Out ~ Tumor_KP$Age + Tumor_KP$Sex + Tumor_KP$Marrow + Tumor_KP$Lung + Tumor_KP$Pleura + Tumor_KP$Supra
                 +Tumor_KP$Skin + Tumor_KP$Axil + Tumor_KP$Media + Tumor_KP$Brain,
                 family="binomial", data=Tumor_KP, na.action=na.omit)

stp_UM2_glm_KP <- step(UM2_glm_KP, direction = "forward")

summary(stp_UM2_glm_KP)

#The reason I chose Lung and Liver variable to eliminate is that Lung was positively co-related before and after applying forward selection model it became negatively co-related; Moreover, Liver variable was also notably changed.
#When it comes to comparing two models, User Model 1 is better than User Model 2. The reasons are stated below:
#3.2.1 AIC : AIC of User Model 2 is lower than Model 1.(Though AIC of Model 2 is lower, the supreme model will be decided by other factors as well)
#3.2.2 Deviance : Residual Deviance of model 2 is lower than model 1.
#3.2.3 Residual Symmetry : These residuals do not help that much to derive which model is better.
#3.2.4 Z-values : Z- Value of variables of Model 1 is better here than Model 2
#3.2.5 Variable Co-efficient : Variable Co-efficient does not help that much to differentiate these two models. 


##############################
##    Model Evaluation      ##
##############################

#Answer 4.1
#For User Model 1

Res_SW_UM1_KP <- predict(stp_UM1_glm_KP, type="response") 
head(Res_SW_UM1_KP,20)

Class_SW_UM1_KP <- ifelse(Res_SW_UM1_KP > 0.5,1,0)        
head(Class_SW_UM1_KP)

True_log_UM1_KP <- Tumor_KP$Brain                        
T1_KP <- table(True_log_UM1_KP, Class_SW_UM1_KP, dnn=list("Act Brain","Predicted") )
T1_KP

#Accuracy of User Model 1    : 256/300 = 0.8533
#Specificity of User Model 1 : 151/190 = 0.7947
#Sensitivity of User Model 1 : 105/110 = 0.9545
#Precision of User Model 1   : 105/144 = 0.7291

#For User Model 2
Res_SW_UM2_KP <- predict(stp_UM2_glm_KP, type="response") 
head(Res_SW_UM2_KP,20)

Class_SW_UM2_KP <- ifelse(Res_SW_UM2_KP > 0.5,1,0)        
head(Class_SW_UM2_KP)

True_log_UM2_KP <- Tumor_KP$Brain                        
T2_KP <- table(True_log_UM2_KP, Class_SW_UM2_KP, dnn=list("Act Brain","Predicted") )
T2_KP

#Accuracy of User Model 2    : 237/300 = 0.7900
#Specificity of User Model 2 : 139/190 = 0.7315
#Sensitivity of User Model 2 : 98/110 = 0.8909
#Precision of User Model 2   : 98/149 = 0.6577

#ROC Curve and AUC of User Model 1

plot(roc(Tumor_KP$Brain,Res_SW_UM1_KP, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Brain Scan (Model 1)')

auc(Tumor_KP$Brain, Res_SW_UM1_KP)

#ROC Curve and AUC of User Model 2

plot(roc(Tumor_KP$Brain,Res_SW_UM2_KP, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Brain Scan (Model 2)')

auc(Tumor_KP$Brain, Res_SW_UM2_KP)

#User Model 1 has higher AUC value therefore It is preferred.

#Answer 5
#Looking at the Confusion matrix, ROC curve and AUC, Model 1 is recommendable. 


###############################
##          PART : B         ##
###############################

#Answer 1.1

start_time_KP <- Sys.time()

Tumor_glm_KP = glm(Tumor_KP$Brain ~ Tumor_KP$Age + Tumor_KP$Sex + Tumor_KP$Bone + Tumor_KP$Marrow + Tumor_KP$Lung + Tumor_KP$Pleura + Tumor_KP$Liver 
                   +Tumor_KP$Skin + Tumor_KP$Supra + Tumor_KP$Axil + Tumor_KP$Media + Tumor_KP$Out,
                   family="binomial", data=Tumor_KP, na.action=na.omit)

stp_Tumor_glm_KP <- step(Tumor_glm_KP, direction = 'forward')

end_time_KP <- Sys.time()

Time_Diff_KP <- end_time_KP - start_time_KP

summary(stp_Tumor_glm_KP)

#Answer 1.2
resp_stp_KP <- predict(stp_Tumor_glm_KP, type="response")  
head(resp_stp_KP,20)

Class_stp_KP <- ifelse(resp_stp_KP > 0.5,1,0)           
head(Class_stp_KP)

True_log_stp_KP <- Tumor_KP$Brain
T1_stp_KP <- table(True_log_stp_KP, Class_stp_KP, dnn=list("Act Brain","Predicted") ) 
T1_stp_KP

#Answer 1.3
Time_Diff_KP


##################################
##  Naive-Bayes Classification  ##
##################################

#Answer 2.1

Tumor_KP$Brain <- as.factor(Tumor_KP$Brain)

#Answer 2.2
start_time_NB_KP <- Sys.time()

Tumor_Naive_KP <- NaiveBayes(Tumor_KP$Brain ~ Tumor_KP$Age + Tumor_KP$Sex + Tumor_KP$Bone + Tumor_KP$Marrow + Tumor_KP$Lung + Tumor_KP$Pleura + Tumor_KP$Liver 
                             +Tumor_KP$Skin + Tumor_KP$Supra + Tumor_KP$Axil + Tumor_KP$Media + Tumor_KP$Out,
                             data=Tumor_KP, na.action=na.omit)

end_time_NB_KP <- Sys.time()

NB_Time_KP <- end_time_NB_KP - start_time_NB_KP

#Answer 2.3
#Classifies
pred_bay_KP <- predict(Tumor_Naive_KP)

#Creates Confusion Matrix
CF_NB_KP <- table(Actual=Tumor_KP$Brain, Predicted=pred_bay_KP$class)

#Answer 2.4
NB_Time_KP


##################################
## Linear Discriminant Analysis ##
##################################

#Answer 3.2
start_time_LDA_KP <- Sys.time()

Tumor_LDA_KP <- lda(Tumor_KP$Brain ~ Tumor_KP$Age + Tumor_KP$Sex + Tumor_KP$Bone + Tumor_KP$Marrow + Tumor_KP$Lung + Tumor_KP$Pleura + Tumor_KP$Liver 
                    +Tumor_KP$Skin + Tumor_KP$Supra + Tumor_KP$Axil + Tumor_KP$Media + Tumor_KP$Out,
                    data=Tumor_KP, na.action=na.omit)
end_time_LDA_KP <- Sys.time()

LDA_Time_KP <- end_time_LDA_KP - start_time_LDA_KP

#Answer 3.3
#Classifies
pred_LDA_KP <- predict(Tumor_LDA_KP, data=Tumor_KP)

#Confusion Matrix
CF_LDA_KP <- table(Actual=Tumor_KP$Brain, Predicted=pred_LDA_KP$class)

#Answer 3.4
LDA_Time_KP


#########################################
## Comparison of all three classifiers ##
#########################################

#Answer 4

T1_stp_KP
CF_NB_KP
CF_LDA_KP

Time_Diff_KP
NB_Time_KP
LDA_Time_KP


#Confusion Metrics of Logistic Regression Model

#Accuracy of LR    : 228/300 = 0.7600
#Sensitivity of LR : 80/110 = 0.7272
#Specificity of LR : 148/190 = 0.7789
#Precision of LR   : 80/122 = 0.6557
#Time Difference of LR : 0.014961 secs


#Confusion Metrics of Naive Bayes Model

#Accuracy of NB    : 193/300 = 0.6433
#Sensitivity of NB : 71/110 = 0.6454 
#Specificity of NB : 122/190 = 0.6421
#Precision of NB   : 71/139 = 0.5107
#Time Difference of NB : 0.01396203 secs


#Confusion Metrics of Linear Discriminant Model

#Accuracy of LDA    : 229/300 = 0.7633
#Sensitivity of LDA : 83/110 = 0.7545
#Specificity of LDA : 146/190 = 0.7684
#Precision of LDA   : 83/127 = 0.6535
#Time Difference of LDA : 0.3372622 secs

#Answer 4.1
#Linear Discriminant classifier is more accurate

#Answer 4.2
#Naive Bayes is most suitable classifier when processing speed is more important

#Answer 4.3
#Logistic Regression has the fewest type 1 Error

#Answer 4.4 
#Linear Discriminant has the fewest type 2 error

#Answer 4.5 
#Comparing these 3 classifiers, Linear Discriminant outweighs other classifiers 

#Answer 4.6
#These classifiers help to choose the best classifying method for the model that we have created in Part 1