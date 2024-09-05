#import data
#load in libraries
library("gmodels")
library("Hmisc")
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")
library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("IDPmisc")
Diabetes <- Diabetes5050
nrow(Diabetes)
ncol(Diabetes)
head(Diabetes)
summary(Diabetes)

counts1 <- table(Diabetes$Diabetes_binary)
barplot(counts1, main='Diabetes distribution',
        xlab= 'frequency of non-diabetics vs diabetics')
### This is significant so bmi should just be a second independent variable rather than a covariate since it does impact whether or not someone with diabetes would have mental health problems. 


# for the Diabetes_binary column 0 represents not having diabetes and 1 represents having diabetes or prediabetes. 
#data wrangle for both our evaluations. 
# Check the correlation for all data
corDiabetes1 <- rcorr(as.matrix(Diabetes))
corDiabetes1
##check for correlation higher than 0.9 for evaluation questions
Diabetes1 = subset(Diabetes, select = c(Diabetes_binary,Veggies, Fruits, PhysActivity, MentHlth))
corDiabetes <- rcorr(as.matrix(Diabetes1))
corDiabetes
# Check the correlation for all data
corDiabetes1 <- rcorr(as.matrix(Diabetes))
corDiabetes1
##none of the data points have a correlation higher than 0.9


#Testing an ancova for diabetes effect on mental health while controlling for bmi
# make sure Iv is an integer
str(Diabetes$Diabetes_binary)
Diabetes$Diabetes_binary <- as.factor(Diabetes$Diabetes_binary)
str(Diabetes$BMI)
Diabetes$BMI <- as.factor(Diabetes$BMI)
str(Diabetes$MentHlth)
plotNormalHistogram(Diabetes$MentHlth)
Diabetes$MentHlthSQRT <- sqrt(Diabetes$MentHlth)
plotNormalHistogram(Diabetes$MentHlthSQRT)
##Pretty positively skewed so we will log it
Diabetes$MentHlthLog <- log(Diabetes$MentHlth)
Diabetes2 <- NaRV.omit(Diabetes)
plotNormalHistogram(Diabetes2$MentHlthLog)
#Meets assumption of normality
#next assumption is homogeniety of variance
leveneTest(MentHlthLog~Diabetes_binary, data=Diabetes2)
## we pass the assumption of homogeneity of variance. 
## have to test the assumption of Homogeneity of Regression Slopes
Homogeneity_RegrSlp = lm(MentHlthLog~BMI, data=Diabetes2)
anova(Homogeneity_RegrSlp)

# Question 1 How does eating fruits and veggies at least once a day impact whether someone has diabetes
## No data wrangling it is already in a usable format: categorical with 0 being no and 1 being that they do eat a fruit or a vegetable  or both once a day
##independent chi square
CrossTable(Diabetes$Fruits, Diabetes$Diabetes_binary, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
CrossTable(Diabetes$Veggies, Diabetes$Diabetes_binary, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
## we pass the assumptions of a frequency five or greater for both tests
## based off of th p values for the Pearson's Chi-squared test both fruits and veggies do impact whether someone has diabetes
##Post hocs analysis:
## Those who eat vegetables once a day are more likely to not have diabetes with a standardized residual of 6.851
## Those who eat fruits once a day are more likely to not have diabetes with a standardized residual of 6.334

#Question two Does good mental health and physical activity impact whether or not someone has diabetes and if so what has a greater impact on that result?
## for physical activity 0 represents not having done any exercise in the last 30 days not including jobs, and 1 means they have exercised in the last 30 days not including there job.
## have to recode mental health into 2 categories, good mental health: 7 days or less of poor mental health = 0, and bad mental health: 8 days or more of bad mental health = 1
Diabetes$Gmntl <- NA
Diabetes$Gmntl[Diabetes$MentHlth == '0'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '1'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '2'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '3'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '4'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '5'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '6'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '7'] <- 0
Diabetes$Gmntl[Diabetes$MentHlth == '8'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '9'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '10'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '11'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '12'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '13'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '14'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '15'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '16'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '17'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '18'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '19'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '20'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '21'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '22'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '23'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '24'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '25'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '26'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '27'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '28'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '29'] <- 1
Diabetes$Gmntl[Diabetes$MentHlth == '30'] <- 1

## This test will also be an independent chi square test so we must test for assumptions of expected frequencies greater than 5
CrossTable(Diabetes$Gmntl, Diabetes$Diabetes_binary, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS") 
CrossTable(Diabetes$PhysActivity, Diabetes$Diabetes_binary, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
##both pass the assumptions necessary to go forward
##Both have a p-value under 0.05 so they both do impact whether or not someone has diabetes 
###post hocs 
## Good mental health means you are more likely to not have a diabetes with a standardized residual of 6.158
## Having worked out in the last 30 days seemed to have more of a significant impact on whether or not someone has diabetes with a standardized residual of 16.256

# Logistical regression to predict whether or not someone has diabetes based off if they have good mental health (7 days or less of bad mental health days)
mylogit <- glm(Diabetes_binary ~ Gmntl, data=Diabetes, family="binomial")
# Make sure we pass all five assumptions for logistical regression

## First assumption of meeting the minimum sample size
###Predict Diabetes or no Diabetes
probabilities <- predict(mylogit, type = "response")
Diabetes$Predicted <- ifelse(probabilities > .5, "pos", "neg")
### recode predicted variable 
Diabetes$PredictedR <- NA
Diabetes$PredictedR[Diabetes$Predicted=='pos'] <- 1
Diabetes$PredictedR[Diabetes$Predicted=='neg'] <- 0
### change variables to factors
Diabetes$PredictedR <- as.factor(Diabetes$PredictedR)
Diabetes$Diabetes_binary <- as.factor(Diabetes$Diabetes_binary)
### create a confusion matrix
Conf_mat <- caret::confusionMatrix(Diabetes$PredictedR, Diabetes$Diabetes_binary)
Conf_mat
## With the confusion matrix we have met the assumption of minimum sample size

## check for assumption of logit linearity
DiabetesR <- Diabetes1 %>%
  dplyr::select_if(is.numeric)

predictors <- colnames(DiabetesR)
DiabetesR <- DiabetesR %>%
  mutate(logit=log(probabilities/(1-probabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)
###Now that we have the logit we can graph and assess the linearity
ggplot(DiabetesR, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
##no line we do not pass that assumption. 

## check for assumption of multicollinearity
### we only have on Independent variable so we don't have to worry about that

## check assumption of independent error by graphing residuals over the index
plot(mylogit$residuals)
## alternatively use the durban-Watson Test
dwtest(mylogit, alternative="two.sided")
###we fail the assumption of independent error

## screen for outliers
liers <- influence.measures(mylogit)
summary(liers)
###There is no outliers so we should be good

#run the logistical regression
summary(mylogit)


#graph the logistical regression
logi.hist.plot(Diabetes$Gmntl, Diabetes$Diabetes_binary, boxp=FALSE, type="hist", col="gray")


# create random forest in python