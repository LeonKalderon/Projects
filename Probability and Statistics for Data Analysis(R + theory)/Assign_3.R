#EXERCISE 1
ba <- c( 6.88, 7.46, 7.34, 7.21, 8.22, 8.42, 6.41, 8.04, 7.35, 8.30, 8.09, 6.68, 8.77, 7.33, 7.15)
be <- c(17.24, 18.06, 17.41, 17.60, 18.95, 19.60, 17.49, 18.60, 17.50, 19.24, 18.87, 17.68, 19.31, 18.40, 17.20) 

#(A)
# A scatterplot to see if there is a linear relationship between the variables
plot(be,ba,main="Vathmos Eisagwgis vs Vathmos apofoithshs", xlab="eisagwgh", ylab="apofoithsh", pch=19)

#correlations between dependent and independent variables
cor(ba, be)

#Test if the correlation between dependent and independent variables is stastically significant
#we reject H0 that says correlation = 0, so there is statistical significant correlation
cor.test(ba, be)

#Simple linear model
fit<-lm(ba ~ be)
summary(fit)
# a = -5.96 , b = 0.74. Both are statistically significant 
# ba = -5.96 + 0.74*be
# R=0.79 . The model inteprets 79% of the actual grades. 
# RSE = sqrt(MSE) = sqrt(s^2) = 0.32 . This is the mean standard error of the model!

#Visualise the predicted values with the true values
plot(be, ba, main="Vathmos eisagwgis vs apofoithshs along with the OLS line", xlab="eisagwghs", ylab="apofoithshs",
     pch=19)
abline(fit, col="red") # regression line (y~x)

#anova is equal with t-test on a simple linear regression
anova(fit) # anova table
# pv<5% we reject null hypothesis b=0 (that means the model is good)
#SSReg is the variation attributed to the regression model
#SSE is the unexplained by the regression model variation, called error.
#SST = SSReg + SSE and R_square = SSReg/ SST
#MSReg = SSReg / Df and MSE = SSE / Df
#Finally, F-value = MSReg/MSE. A big F-value indicates a small p-value, consequently a good model!


#(B)
#test for the normality of the residuals
qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)

#Shapiro-Wilk normality test
#pv = 17% we do not reject H0, that says that the residuals follow normality
shapiro.test(fit$residuals)

#install.packages("nortest")
library(nortest)
#same here  with pv = 21%
lillie.test(fit$residuals)

par(mfrow=c(2,2)) ### provide plots in a 2x2 layout
#Visualize the assumptions for a valid linear model.
plot(fit)
#The top-left and top-right graphs are the most important one, 
#the top-left graph check for the homogeneity of the variance and the linear relation,
#if you see no pattern in this graph (ie if this graph looks like stars in the sky), 
#then your assumptions are met. The second graphs check for the normal distribution of the residuals,
#the points should fall on a line.

#Test for homoscedasticity
#install.packages('car')
#library(car)
# non-constant error variance test
#we do not reject homoscedasticity assumption.
ncvTest(fit)

# (C)
#95% confidence intervals for for model parameters
confint(fit, level=0.95)

#Predicted value point estimate for x=18.5 (y_hat = 7.92) and CI for this prediction
predict(fit, data.frame(be = 18.5), interval="prediction", level = 0.95)

#CI for the mean (a_hat + b_hat) of x=18.5
predict(fit, data.frame(be = 18.5), interval="confidence", level = 0.95)


# (D)
be_squared <- be^2
fit2<-lm(ba ~ be + be_squared)
summary(fit2)
#All the pv are statistically unsignificant this is not valid model

#(5)
lnba<-log(ba, base=exp(1))
fitln<-lm(lnba ~ be)
summary(fitln)
#R squared = Explained variation / Total variation = 0.78
#The model is not good, intercept's p-value=0.355>0.05, thus it is not statistical significant. 

#All in all the first model seems to be the best.

#EXERCISE 2
#read the data
dataall<-read.table("C:/Users/User/Desktop/Probability and statistics for data analysis/Assign_3/Assignment3-askisi2.txt")

# we will run all the models and we will see which one is the best!
#1rst researcher
fit1 <- lm(dataall$V1 ~ dataall$V2 + dataall$V5 + dataall$V6)
summary(fit1)
#our first model has a good R_squared but the variable V5 is not statistically significant

#2nd researcher
fit2 <- lm(dataall$V1 ~ dataall$V2+dataall$V4+dataall$V5)
summary(fit2)
#this is a very good model with accuracy(R^2)=87%. Even though, our last variable is not statistically significant for a=0,05

#3rd researcher
fit3 <- lm(dataall$V1 ~ dataall$V2 + dataall$V4)
summary(fit3)
#The third is the only model with all 'b' being statistically significant for a=0.05.
#Also it has a great R_square = 85.2%.

#All in all for a = 0.05 we should select the 3rd researcher. 
#Even though, for a=0.10, 2nd's researcher model is slightly better.


#EXERICISE 3
# (A)
dataall<-read.table('C:/Users/User/Desktop/Probability and statistics for data analysis/Assign_3/Assignment3-askisi3.txt')

lnGDP<-log(dataall$V2)
lnC <- log(dataall$V3)
lnT <- log(dataall$V4)

# A scatterplot to see if there is a linear relationship between the variables
par(mfrow=c(1,1))
plot(lnGDP, lnC, main="(GDP vs Capital", xlab="Capital", ylab="GDP", pch=19)
#With a quick look we see that there is not a linear relationship between these variables.

#correlations between dependent and independent variables
cor(lnGDP, lnC)

#Test if the correlation between dependent and independent variables is stastically significant
#we do not reject H0 that says that correlation = 0, so there is NOT statistical significant correlation
cor.test(lnGDP, lnC)

#Run our model
fitln<-lm(lnGDP ~ lnC)
summary(fitln)
#for a=0.05 we do not reject null hypothesis for slobe (b) that says that b=0, thus our model is not good
#for a=0.05 we reject the hypothesis that says that intercept(a) =0.
#Also our R_square is 6%.

anova(fitln)
#An??va's table results are just as bad. Our model can exaplain a small variation of the fitted values.
#Also, we can see again that p-value for b is 0.1093

#All in all this is a useless model.
#Even though, just for the practise we will test the assumptions.

#test for the normality of the residuals
qqnorm(fitln$residuals,main="NPP for residuals")
qqline(fitln$residuals,col="red",lty=1,lwd=2)

#Shapiro-Wilk normality test
shapiro.test(fitln$residuals)
#pv = 7% we do not reject H0, that says that the residuals follow normality.(for a = 0.05)

#Test for homoscedasticity
ncvTest(fitln)
#pv = 0.13 we do not reject homoscedasticity assumption.

#Visualise the assumptions
par(mfrow=c(2,2))
plot(fitln)
 

#To conclude, as mentioned above, this model is invalid for a=0.05 but the assumptions are valid.

# (B)

#M1: the model of question (A)

#M2: 
fitln2<-lm(lnGDP ~ lnT)
summary(fitln2)
#the parameteres of the model are not statistically significant

#M3
fitln3<-lm(lnGDP ~ lnC + lnT)
summary(fitln3)
#One more time, the parameteres of the model are not statistically significant for a=0.05

#None of the model is good. We should use SLR at all to predict gdp
#However, if we had to choose one of the three models, the first one is the best (M1).

#To predict GPD for C=5000000 and T=3500000 we need the third model because it is the only one that takes both parameters.
y_hat = 17.2134 + 0.2575*log(5000000) - 0.2844* log(3500000)
GDP_hat = exp(y_hat)

GDP_hat





