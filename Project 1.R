#STAT 6021 Project 1 
#group 10

#objectives
#find a suitable model to relate gas mileage to the given predictors 
#1-explore relationships between some of the predictors with the response variable 
#2-find a model that fits well
#3-find a simple model 

#Predictor Summary
# y: gas mileage (miles/gallon)
# x1: Displacement (cubic in.)
# x2: Horsepower (ft-lb)
# x3: Torque (ft-lb)
# x4: Compression ratio
# x5: Rear axle ratio
# x6: Carburetor (barrels)
# x7: No. of transmission speeds
# x8: Overall length (in.)
# x9: Width (in.)
# x10: Weight (lb)
# x11: Type of transmission (automatic/manual)
options(max.print=1000000)
library(MASS)
library(leaps)

data<-read.table("mileage.txt", header=TRUE, sep = "")
attach(data)
head(data)

#make this predictor categorical 
x11<-factor(x11) 
is.factor(x11)

#create scatterplot of all predictors to look for correlations 
data_subset<-data[,0:11]
pairs(data_subset, lower.panel = NULL, main="Scatterplot of Quantitative Variables")

cor(data_subset)

#####################
##intercept only model
regnull <- lm(y~1, data=data)
##model with all predictors
regfull <- lm(y~., data=data)

##forward selection, backward elimination, and stepwise regression
step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
#output suggests y~x1+x6 is good fit
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
#output suggests y~X5+x8+x10 is good fit
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")
#output suggests y~x1+x6 is good fit

#based on these outputs we will test the regression assumptions of model 1: y~x1+x6 and model 2: y~x5+x8+x10
#residuals, ACF, qq, box cox
#if satisfactory: check VIF, R2, R2adj, 

#######################################################################################################################
#checking regression assumptions of model 1
model_1<-lm(y~x1+x6)

#explore results
summary(model_1) #x6 has a p value > 0.05 - will need to perform a hypothesis test to determine if we can drop the predictor

#ANOVA test of full model 
anova(model_1) #x6 p value > 0.05

par(mfrow=c(2,2))
##residual plot of model with no interaction
plot(model_1$fitted.values,model_1$residuals,main="Residual plot") #residual plot appears to have a shape and non-constant variance
abline(h=0,col="red")

##ACF plot of residuals
acf(model_1$residuals) #assumptions met

##QQ plot of residuals
qqnorm(model_1$residuals) #assumptions met
qqline(model_1$residuals, col="red")

#check to see if our model might need a transform
boxcox(model_1) #0 within confidence interval may need to transform

#model 1 hypothesis test 
#partial F test to remove x6
#H0: x6 coefficient = 0
#Ha: x6 coefficient not = 0
reduced<-lm(y~x1)
anova(reduced, model_1) #resultant p value is 0.1631 which is > 0.05 therefore we fail to reject the null and x6 can be removed from the model

par(mfrow=c(2,2))
#model_1 is now y~x1 we must test the regression assumptions for the reduced model
##residual plot of model with no interaction
plot(reduced$fitted.values,reduced$residuals,main="Residual plot") #residual plot appears to have a shape and non-constant variance
abline(h=0,col="red")

##ACF plot of residuals
acf(reduced$residuals) #assumptions met

##QQ plot of residuals
qqnorm(reduced$residuals) #assumptions met
qqline(reduced$residuals, col="red")

#check to see if our model might need a transform
boxcox(reduced) #0 within confidence interval may need to transform

#transform model_1 with log
trans_mod_1 <- log(y)
log.result <- lm(trans_mod_1~x1)
summary(log.result)

par(mfrow=c(2,2))
#check regression assumptions
plot(log.result$fitted.values,log.result$residuals,main="Residual plot") #residual plot appears to have a shape and non-constant variance
abline(h=0,col="red")

##ACF plot of residuals
acf(log.result$residuals) #assumptions met

##QQ plot of residuals
qqnorm(log.result$residuals) #assumptions met
qqline(log.result$residuals, col="red")

#check to see if our model might need a transform
boxcox(log.result) #0 within confidence interval may need to transform


PRESS <- function(log.result) {
  ## get the residuals from the linear.model. extract hat from lm.influence to obtain the leverages
  pr <- residuals(log.result)/(1-lm.influence(log.result)$hat)
  ## calculate the PRESS by squaring each term and adding them up
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

PRESS(log.result) # returns 0.6245769
#######################################################################################################################

#Model 2 
model_2<-lm(y~x5+x8+x10)
#explore results
summary(model_2) #all p values under 0.05, reject the null hypothesis, all predictors are signifcant
#regression equation: y= 5.011 + 2.625x5 + 0.212x8 - 0.009x10

#anova test of model 2
anova(model_2) ##all p values < 0.05

par(mfrow=c(2,2))
#residual plot of model 2 with no interaction
plot(model_2$fitted.values,model_2$residuals,main="Residual plot") #plot indicated non-constant variance
abline(h=0,col="red")

#acf plot of residuals
acf(model_2$residuals) #assumptions met

#qq plot of residuals 
qqnorm(model_2$residuals) #assumptions met, looks slightly light tailed
qqline(model_2$residuals, col="red")

#check to see if our model might need a transform
boxcox(model_2) #0 within confidence interval so need a transformation

##log transformation
log.model2<-log(y)

result.log<-lm(log.model2~x5+x8+x10)
summary(result.log)

par(mfrow=c(2,2))
plot(result.log$fitted.values,result.log$residuals, xlab="Fitted Values", ylab="Residuals", main="Residual Plot")
abline(h=0, col="red") ##the variance looks better in this residual plot

acf(result.log$residuals) #assumptions met

qqnorm(result.log$residuals)
qqline(result.log$residuals, col="red") ##qq plot looks more normal now

boxcox(result.log, lambda = seq(-4, 4, 1/10)) ##contains 1 in the interval so no more transformation needed to y 

PRESS(result.log) #returns 0.6100152




