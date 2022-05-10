#install packages if needed
install.packages("MVN")
install.packages("mvnormtest")
install.packages("ggplot2")
install.packages("biotools")
install.packages("MASS")
install.packages("dplyr")
install.packages("candisc")
install.packages("car")
install.packages("cowplot")

#load packages if needed
library(MVN)
library(mvnormtest)
library(ggplot2)
library(biotools)
library(MASS)
library(dplyr)
library(candisc)
library(car)
library(cowplot)
library(readxl)
library(nnet)
library(lmtest)
library(ggplotgui)
library(shiny)
library(psych)
library("ez") 

#Set working directory if needed

#Import data
Biometrics_Survey_Data <- data.frame(read.csv("Biometrics_Survey_Data.csv"))
View(Biometrics_Survey_Data)
names(Biometrics_Survey_Data)


#remove NAs
Biometrics_Survey_Data <- na.omit(Biometrics_Survey_Data) 

#examine data
ggplot_shiny(Biometrics_Survey_Data)
#check assumptions normality,collinality 

#Check Assumptions----
#Histogram by Groups
#Create separate data for each group
eye_scan <- subset(Biometrics_Survey_Data, 
                   AuthMethod=="eye_scan")
face_recognition <- subset(Biometrics_Survey_Data, 
                           AuthMethod=="face_recognition")
fingerprint <- subset(Biometrics_Survey_Data, 
                      AuthMethod=="fingerprint")
hand_geometry <- subset(Biometrics_Survey_Data, 
                        AuthMethod=="hand_geometry")
password <- subset(Biometrics_Survey_Data, 
                   AuthMethod=="password")
PIN <- subset(Biometrics_Survey_Data, 
              AuthMethod=="PIN")
voice <- subset(Biometrics_Survey_Data, 
                AuthMethod=="voice")
#Check new data frames
table(Biometrics_Survey_Data$AuthMethod)

##############################################################################
######differences in perceptions of usefulness, ease of use, convenience,##### 
######and trust based on the type of authentication method?###################
##############################################################################



#Create Matrix with Dep. Vars.
Dep_matrix <- as.matrix(Biometrics_Survey_Data[ ,c("Perceived_Usefulness", "Ease_Of_Use", "Convenience", "Trust")])
View(Dep_matrix)

#Check Assumptions----
##Plot Multivariate Normality----
mvn(Dep_matrix, 
    multivariatePlot="qq")

mvnObj <- mvn(
  data=Biometrics_Survey_Data[ ,c( "Perceived_Usefulness", "Ease_Of_Use", "Convenience", "Trust")],
  mvnTest="royston",
  univariateTest = "Lillie",
  multivariatePlot = "qq",
  multivariateOutlierMethod = "adj",
  showOutliers = TRUE,
  showNewData = TRUE)


#from the graph produced by the above, are there outliers? If not then the next 2 lines are not necessary; 
#if there are, then we would use the data created in the next two lines instead of the original data in the subsequent steps below.
#OutlierRm <- data.frame(mvnObj$newData) #so if there are outliers use "OutlierRM" as the data below instead of HBAT_200
#View(OutlierRm)
#remove outliers
mvnObj[["multivariateOutliers"]][["Observation"]]
Biometrics_Survey_Data <- Biometrics_Survey_Data[-c(91,180,14,2,110,143,36,125,173,178,147,144,31,88,11,15,108,154,131,158,127,6,121,100,21),]

#Create Matrix with Dep. Vars.
Dep_matrix <- as.matrix(Biometrics_Survey_Data[ ,c("Perceived_Usefulness", "Ease_Of_Use", "Convenience", "Trust")])
View(Dep_matrix)

#Check Assumptions----
##Plot Multivariate Normality----
mvn(Dep_matrix, 
    multivariatePlot="qq")

##Plot multivariate normality by group----
#Create DF with Dep. Vars. and grouping var
Mod_Vars <- as.data.frame(Biometrics_Survey_Data[ ,c("AuthMethod_Type","Perceived_Usefulness", "Ease_Of_Use", "Convenience", "Trust")])

#Normality by Group
NormTestByGrp <- mvn(Mod_Vars,
                     subset="AuthMethod_Type",
                     mvnTest="royston",
                     univariateTest="Lillie")
NormTestByGrp$multivariateNormality

#test homogeneity of variances
boxM( cbind(Perceived_Usefulness, Ease_Of_Use, Convenience, Trust) ~ AuthMethod_Type, data=Biometrics_Survey_Data)


#Are there differences in perceptions of usefulness, ease of use, convenience, and trust based on the type of authentication method? 
#MANOVA----
ManovaOut <- manova(Dep_matrix~
                      AuthMethod_Type, data = Biometrics_Survey_Data)
Manova(ManovaOut, type = 3)
summary(ManovaOut)
#
#Follow-up ANOVAs (Type III SS)
Perceived_Usefulness_aov <- aov(
  Perceived_Usefulness~AuthMethod_Type,
  data=Biometrics_Survey_Data)
Anova(Perceived_Usefulness_aov, type=3)
TukeyHSD(Perceived_Usefulness_aov, conf.level = 0.9833)
#significantly different within usefulness
#
Ease_Of_Use_aov <- aov(
  Ease_Of_Use~AuthMethod_Type, 
  data=Biometrics_Survey_Data)
Anova(Ease_Of_Use_aov, type=3)
TukeyHSD(Ease_Of_Use_aov, conf.level = 0.9833)
#significantly different within ease of use
#
Convenience_aov <- aov(
  Convenience~AuthMethod_Type, 
  data=Biometrics_Survey_Data)
Anova(Convenience_aov, type=3)
TukeyHSD(Convenience_aov, conf.level = 0.9833)  
#significantly different within convenience
#
Trust_aov <- aov(
  Trust~AuthMethod_Type, 
  data=Biometrics_Survey_Data)
Anova(Trust_aov, type=3)
TukeyHSD(Trust_aov, conf.level = 0.9833)  
#significantly different within Trust

#Dive into each AuthMethod
#Create DF with Dep. Vars. and grouping var
Mod_Vars <- as.data.frame(Biometrics_Survey_Data[ ,c("AuthMethod","Perceived_Usefulness", "Ease_Of_Use", "Convenience", "Trust")])


#Normality by Group
NormTestByGrp <- mvn(Mod_Vars,
                     subset="AuthMethod",
                     mvnTest="royston",
                     univariateTest="Lillie")
NormTestByGrp$multivariateNormality
#
#Plot univariate histograms & QQ plots

mvn(Dep_matrix[,1:3], 
    univariatePlot="histogram")
mvn(Dep_matrix[,1:3], 
    univariatePlot="qqplot")


#Descriptive Statistics
View(NormTest$Descriptives)
View(NormTestByGrp$Descriptives$`eye_scan`)
View(NormTestByGrp$Descriptives$`face_recognition`)
View(NormTestByGrp$Descriptives$`fingerprint`)
View(NormTestByGrp$Descriptives$`hand_geometry`)
View(NormTestByGrp$Descriptives$`password`)
View(NormTestByGrp$Descriptives$`PIN`)
View(NormTestByGrp$Descriptives$`voice`)



################################################################################
##### adding Computer_Self_Efficacy as covariates###############################
################################################################################

#With a covariate + Computer_Self_Efficacy
Mancova <- manova(Dep_matrix~
                    AuthMethod_Type + Computer_Self_Efficacy, Biometrics_Survey_Data)
Manova(Mancova, type = 3)
summary(Mancova)


#Follow-up ANOVAs (Type III SS)
Perceived_Usefulness_aov <- aov(
  Perceived_Usefulness~AuthMethod_Type + Computer_Self_Efficacy,
  data=Biometrics_Survey_Data)
Anova(Perceived_Usefulness_aov, type=3)
TukeyHSD(Perceived_Usefulness_aov, "AuthMethod_Type", conf.level = 0.9833)

#
Ease_Of_Use_aov <- aov(
  Ease_Of_Use~AuthMethod_Type+Computer_Self_Efficacy, 
  data=Biometrics_Survey_Data)
Anova(Ease_Of_Use_aov, type=3)
TukeyHSD(Ease_Of_Use_aov, "AuthMethod_Type", conf.level = 0.9833)
#
Convenience_aov <- aov(
  Convenience~AuthMethod_Type+Computer_Self_Efficacy, 
  data=Biometrics_Survey_Data)
Anova(Convenience_aov, type=3)
TukeyHSD(Convenience_aov, "AuthMethod_Type", conf.level = 0.9833)  
# no significant difference
#
Trust_aov <- aov(
  Trust~AuthMethod_Type+Computer_Self_Efficacy, 
  data=Biometrics_Survey_Data)
Anova(Trust_aov, type=3)
TukeyHSD(Trust_aov, "AuthMethod_Type", conf.level = 0.9833)  


#################################################################################
#Creating a model to predict someone’s willingness to perform actions 
# with personally identifiable information (intentions-SSN) on a device based 
# on their perceptions of risk, their perceptions of the authentication method 
# (usefulness, ease of use, convenience, and trust), familiarity, past use, and 
# current use of an authentication method 

# Remmove rows with na from the list 
#Biometrics_Survey_Data <- data.frame(read.csv("Biometrics_Survey_Data.csv"))
#Biometrics_Survey_Data_rmna <- na.omit(Biometrics_Survey_Data) 


#Biometrics_Survey_Data$Int_SSN <- ifelse(Biometrics_Survey_Data$Int_SSN=="Yes",1,0)
#Biometrics_Survey_Data$AuthMethod_CurrentUse <- ifelse(Biometrics_Survey_Data$AuthMethod_CurrentUse=="Yes",1,0)


#Run the Binary logistic regression
LogMod <- multinom(
  Int_SSN ~ Perceived_Usefulness + Ease_Of_Use + Convenience + Trust + AuthMethod_Familiarity 
          + AuthMethod_Use + AuthMethod_CurrentUse + Scenario_Risk_SSN,
  data=Biometrics_Survey_Data, family = "binomial")
summary(LogMod)

# Test the goodness of fit
chisq.test(Biometrics_Survey_Data$Int_SSN, predict(LogMod))
lrtest(LogMod)
#model is significant 

#length(predict(LogMod))
#length(Biometrics_Survey_Data$Int_SSN)
#logistic regression results
summary(LogMod)

###########
#MIXED STEP-WISE REGRESSION
#define intercept-only model
intercept_only <- multinom(Int_SSN ~ 1, 
                           data=Biometrics_Survey_Data)

#define model with all predictors
all <- multinom(Int_SSN ~ Perceived_Usefulness + Ease_Of_Use + Convenience + Trust 
                + AuthMethod_Familiarity + AuthMethod_Use + AuthMethod_CurrentUse + Scenario_Risk_SSN, 
                data=Biometrics_Survey_Data)
summary(all)

#perform mixed stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of mixed stepwise regression
both$anova
#perceived_usefulness and authmethod_familiarity are significant IVs to the model
summary(both)

#Estimated Probability of Int_SSN
Biometrics_Survey_Data <- cbind(Biometrics_Survey_Data,both$fitted.values)
View(Biometrics_Survey_Data)

Biometrics_Survey_Data$Pred_Int_SSN <- predict(both)
View(Biometrics_Survey_Data)

#input for estimate
I1 <- data.frame(Perceived_Usefulness=4, Ease_Of_Use=3, Convenience=4, Trust=2, 
                 AuthMethod_Familiarity = 4, AuthMethod_Use = 3, Scenario_Risk_SSN = 3, AuthMethod_CurrentUse = 'No' )
#probs for estimate
probI1 <- predict(all, I1, "probs")
probI1
# 0.802912 

#odds for estimate
oddsI1= probI1/(1-probI1)
oddsI1
#4.073877

#ln(Odds)
coef(both)
#    (Intercept)   Perceived_Usefulness AuthMethod_Familiarity 
#   5.110797            -2.122298             0.436497 
#Odds each National origin car compared to buying a German car
exp(coef(both))
#        (Intercept)   Perceived_Usefulness AuthMethod_Familiarity 
#   165.8024649            0.1197561            1.5472776 

#confusion matrix
xtab <- table(Biometrics_Survey_Data$Int_SSN,
              Biometrics_Survey_Data$Pred_Int_SSN)
xtab
#     No Yes
# No  76  16
# Yes 10  51

# percent improvement
#how many wrong with no model
wrongNM <- sum(xtab)-max(rowSums(xtab)) 
#how many wrong with model
wrongWM <- sum(xtab)-sum(diag(xtab))
#percent improvement in error rate
(wrongNM-wrongWM)/wrongNM
# 0.5737705

#kappa
CPkML <- cohen.kappa(xtab)
CPkML$kappa
#0.6513585

##########################################################
# Using Multi regression with the 1-5 scale to predict Intention score#
##########################################################

#multi regression
MultiReg<- lm( Intentions_SSN  ~ Perceived_Usefulness + Ease_Of_Use + Convenience + Trust 
             + AuthMethod_Familiarity + AuthMethod_Use + AuthMethod_CurrentUse + Scenario_Risk_SSN, 
             data = Biometrics_Survey_Data)
summary(MultiReg)

#Test overall model---- whether the model is significant
anova(MultiReg, test="Chisq")
lrtest(MultiReg)

###########
#MIXED STEP-WISE REGRESSION
#define intercept-only model
intercept_only <- lm(Intentions_SSN  ~ 1, 
                     data=Biometrics_Survey_Data)

#define model with all predictors
all <- lm(Intentions_SSN  ~ Perceived_Usefulness + Ease_Of_Use + Convenience + Trust 
          + AuthMethod_Familiarity + AuthMethod_Use + AuthMethod_CurrentUse + Scenario_Risk_SSN, 
          data=Biometrics_Survey_Data)
summary(all)

#perform mixed stepwise regression
both2 <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#view results of mixed stepwise regression
both2$anova
summary(both2)

#Estimated Probability of Int_SSN
Biometrics_Survey_Data <- cbind(Biometrics_Survey_Data,both2$fitted.values)
View(Biometrics_Survey_Data)

Biometrics_Survey_Data$Pred_Int_SSN1 <- predict(both2)
View(Biometrics_Survey_Data)

#input for estimate
I1 <- data.frame(Perceived_Usefulness=4, Ease_Of_Use=3, Convenience=4, Trust=2, 
                 AuthMethod_Familiarity = 4, AuthMethod_Use = 3, Scenario_Risk_SSN = 2, AuthMethod_CurrentUse = 'No' )
predict(both2, I1, interval="confid")


# estimate
est <- predict(both2, I1)
est
# 3.883536 


#odds for estimate
oddsI1= probI1/(1-probI1)
oddsI1
#4.073877

#ln(Odds)
coef(both)
#   (Intercept)   Perceived_Usefulness AuthMethod_Familiarity 
#  5.110797            -2.122298             0.436497 

exp(coef(both))
#  (Intercept)   Perceived_Usefulness AuthMethod_Familiarity 
# 165.8024649            0.1197561            1.5472776 





