#install.packages('AER')
#install.packages('glmnet')
#install.packages('readtext')
# Load readtext package
#library(readtext)
#install.packages('mpath')
#install.packages('glmnetNB')
library(mpath)
library(glmnet)
library("AER")

#======ETL(EXTRACT-TRANSFORM-LOAD)=============
file = 'C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Homework3/CommViolPredUnnormalizedData.txt'
DF = read.csv(file, header = TRUE, sep = ",", dec = ".", na.strings='?')
DF = as.data.frame(DF)
dim(DF)

#Because the plain txt file does not contain Feature names we copied the names to vim and with easy
#transormations they are ready to be added to our DFframe.
fileColNames = 'C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Homework3/ColNames.csv'
colNames = read.csv(file = fileColNames)
colnames(DF) = t(colNames)


# Get rid of non-predictive columns
DF$State = NULL
DF$countyCode = NULL
DF$communityname = NULL
DF$communityCode = NULL
DF$fold = NULL
DF$pop = NULL
# After reading the documentation of the dataset we found out that these variables could be excluded from our analysis
# However, we run our analysis without droping them
# DF$numbUrban = NULL
# DF['pct12-21'] = NULL
# DF['pct16-24'] = NULL
# DF$medFamIncome = NULL
# DF$NumUnderPov = NULL
# DF$whitePerCap = NULL
# DF$blackPerCap = NULL
# DF$NAperCap = NULL
# DF$asianPerCap = NULL
# DF$otherPerCap = NULL
# DF$hispPerCap = NULL
# DF$pctEmploy = NULL
# DF$pctOccupManu = NULL
# DF$pctMaleDivorc = NULL
# DF$pctFemDivorc = NULL
# DF$pct2Par = NULL
# DF['pctKids-4w2Par'] = NULL
# DF['pct12-17w2Par'] = NULL
# DF['pctWorkMom-6'] = NULL
# DF['kidsBornNevrMarr'] = NULL
# DF['numForeignBorn'] = NULL
# DF['pctImmig-3'] = NULL
# DF['pctImmig-5'] = NULL
# DF['pctImmig-10'] = NULL
# DF['pctFgnImmig-3'] = NULL
# DF['pctFgnImmig-5'] = NULL
# DF['pctFgnImmig-10'] = NULL
# DF$pctSpeakOnlyEng = NULL
# DF$pctLargHous = NULL
# DF$persPerOwnOccup = NULL
# DF$persPerRenterOccup = NULL
# DF$pctPopDenseHous = NULL
# DF$pctSmallHousUnits = NULL
# DF$pctVacant6up = NULL
# DF$ownHousLowQ = NULL
# DF$ownHousUperQ = NULL
# DF$ownHousQrange = NULL
# DF$rentLowQ = NULL
# DF$rentUpperQ = NULL


#We drop these columns because they are >90% NAs 
DF$numPolice = NULL
DF$policePerPop = NULL
DF$policeField = NULL
DF$policeFieldPerPop = NULL
DF$policeCalls = NULL
DF$policCallPerPop = NULL
DF$policCallPerOffic = NULL
DF$policePerPop2 = NULL
DF$racialMatch = NULL
DF$pctPolicWhite = NULL
DF$pctPolicBlack = NULL
DF$pctPolicHisp = NULL
DF$pctPolicAsian = NULL
DF$pctPolicMinority = NULL
DF$officDrugUnits = NULL
DF$numDiffDrugsSeiz = NULL
DF$policAveOT = NULL
DF$policCarsAvail = NULL
DF$policOperBudget = NULL
DF$pctPolicPatrol = NULL
DF$gangUnit = NULL
DF$policBudgetPerPop = NULL


# Get rid of all predictive columns except Murders which is the goal variable!
DF$ViolentCrimesPerPop = NULL
DF$murdPerPop = NULL
DF$rapes = NULL
DF$rapesPerPop = NULL
DF$robberies = NULL
DF$robbbPerPop = NULL
DF$assaults = NULL
DF$assaultPerPop = NULL
DF$burglaries = NULL
DF$burglPerPop = NULL
DF$larcenies = NULL
DF$larcPerPop = NULL
DF$autoTheft = NULL
DF$autoTheftPerPop = NULL
DF$arsons = NULL
DF$arsonsPerPop = NULL
DF$nonViolPerPop = NULL
DF$violentPerPop = NULL

DF = na.omit(DF)
DF = apply(DF, 2, as.numeric)
DF = as.data.frame(DF)



#=====GLM-REGULARIZATION-POISSON==========
#Poisson regression is used to model count data under the assumption of Poisson error (murders is a positive integer number),
#or otherwise non-negative data where the mean and variance are proportional. 
#Like the Gaussian and binomial model, the Poisson is a member of the exponential family of distributions. 


# Build Regularized models, glmnet() finds the coefficients for different values of lambda (l)
#Alpha takes values 0<a<1 from the user. Ridge: a=0 , Lasso: a=1 and ElasticNet: a=0.5
formula <- model.matrix(murders~., DF)[,-1]

#we can define our own sequence of possible lambdas
#after some trial and error we concluded that the default lambdas give better results
#lambdas <- 2^seq(-4,10,length=500)
lambdas <- seq(10^-2,100,length=200)

#create GLM's for different alphas and lambdas
lasso_glmPoisson = glmnet(formula, DF$murders, lambda = lambdas, alpha=1, family="poisson")
elasticnet_glmPoisson = glmnet(formula, DF$murders, lambda = lambdas, alpha=0.5, family="poisson")
ridge_glmPoisoon = glmnet(formula, DF$murders, alpha=0, family="poisson")


#with these plots we can see how the value of the coefficients are changing for different lambdas
par(mfrow = c(2,2))
plot(lasso_glmPoisson, xvar="lambda",label=TRUE)
title(main="LASSO", line=2)
plot(elasticnet_glmPoisson, xvar="lambda",label=TRUE)
title(main="ELASTIC_NET", line=2)
plot(ridge_glmPoisoon, xvar="lambda",label=TRUE)
title(main="RIDGE", line=2)
#cv.glmnet: we use this function to do cross-validation and
#find the optimal lambda according to different criterias like mse, mae, auc, dev etc.
# Cross Validation
Lasso_CV_lambda.cv <- cv.glmnet(formula, DF$murders, alpha=1, lambda = lambdas, family="poisson", type.measure = "mse")
ElasticNet_CV_lambda.cv <- cv.glmnet(formula, DF$murders, alpha=0.5, lambda = lambdas, family="poisson", parallel = TRUE, type.measure = "mse")
Ridge_CV_lambda.cv <- cv.glmnet(formula, DF$murders, alpha=0, lambda = lambdas, family="poisson", parallel = TRUE, type.measure = "mse")

#CV with different type measurements
Lasso_CV_lambda.cv2 <- cv.glmnet(formula, DF$murders, alpha=1, lambda = lambdas, family="poisson", type.measure = "dev")
ElasticNet_CV_lambda.cv2 <- cv.glmnet(formula, DF$murders, alpha=0.5, lambda = lambdas, family="poisson", parallel = TRUE, type.measure = "dev")
Ridge_CV_lambda.cv2 <- cv.glmnet(formula, DF$murders, alpha=0, lambda = lambdas, family="poisson", parallel = TRUE, type.measure = "dev")

Lasso_CV_lambda.cv3 <- cv.glmnet(formula, DF$murders, alpha=1, lambda = lambdas, family="poisson", type.measure = "mae")
ElasticNet_CV_lambda.cv3 <- cv.glmnet(formula, DF$murders, alpha=0.5, lambda = lambdas, family="poisson", parallel = TRUE, type.measure = "mae")
Ridge_CV_lambda.cv3 <- cv.glmnet(formula, DF$murders, alpha=0, lambda = lambdas, family="poisson", parallel = TRUE, type.measure = "mae")

#plots
par(mfrow=c(2,2))
plot(Lasso_CV_lambda.cv);
title(main="LASSO", line=2)
plot(ElasticNet_CV_lambda.cv);
title(main="ELASTICNET", line=2)
plot(Ridge_CV_lambda.cv)
title(main="RIDGE", line=2)
plot(log(Lasso_CV_lambda.cv$lambda),Lasso_CV_lambda.cv$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=Lasso_CV_lambda.cv$name)


#try to find the proper coefficients with different lambdas we got from cv
coef(lasso_glmPoisson, s = Lasso_CV_lambda.cv3$lambda.min)

#Build the final Poisson Regression
modelPoisson <-  glm(murders~., data = DF[,c('houseVacant', 'persPoverty', 'pctKids2Par', 'perHoush', 'medNumBedrm', 'murders')], family = ("poisson"))
summary(modelPoisson)$coefficients
#write the results to a .csv file
write.csv(data.frame(summary(modelPoisson)$coefficients),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Homework3/PoissonReg.csv")

#====REGULARIZATION-LM-UPDATED=====
lasso_glmGaus = glmnet(formula, DF$murders, alpha=1, family="gaussian")
elasticnet_glGaus = glmnet(formula, DF$murders, alpha=0.5, family="gaussian")
ridge_glmGaus = glmnet(formula, DF$murders, alpha=0, family="gaussian")


#with these plots we can see how the value of the coefficients are changing for different lambdas
par(mfrow = c(2,2))
plot(lasso_glmGaus, xvar="lambda",label=TRUE)
title(main="LASSO", line=2)
plot(elasticnet_glGaus, xvar="lambda",label=TRUE)
title(main="ELASTIC_NET", line=2)
plot(ridge_glmGaus, xvar="lambda",label=TRUE)
title(main="RIDGE", line=2)
#cv.glmnet is quite similar with glmnet BUT it does Cross-Validation 
#to find the optimal lambda according to different criterias like mse, mae, auc etc.
# Cross Validation
CV_Lasso_Gaus.cv <- cv.glmnet(formula, DF$murders, alpha=1, lambda = lambdas, family="gaussian", type.measure = "mse")
CV_ElasticNet_Gaus.cv <- cv.glmnet(formula, DF$murders, alpha=0.5, lambda = lambdas, family="gaussian", parallel = TRUE, type.measure = "mse")
CV_Ridge_Gaus.cv <- cv.glmnet(formula, DF$murders, alpha=0, lambda = lambdas, family="gaussian", parallel = TRUE, type.measure = "mse")

#Lasso with different type measurements
CV_Lasso_Gaus.cv2 <- cv.glmnet(formula, DF$murders, alpha=1, lambda = lambdas, family="gaussian", type.measure = "dev")
CV_ElasticNet_Gaus.cv2 <- cv.glmnet(formula, DF$murders, alpha=0.5, lambda = lambdas, family="gaussian", parallel = TRUE, type.measure = "dev")
CV_Ridge_Gaus.cv2 <- cv.glmnet(formula, DF$murders, alpha=0, lambda = lambdas, family="gaussian", parallel = TRUE, type.measure = "dev")

CV_Lasso_Gaus.cv3 <- cv.glmnet(formula, DF$murders, alpha=1, lambda = lambdas, family="gaussian", type.measure = "mae")
CV_ElasticNet_Gaus.cv3 <- cv.glmnet(formula, DF$murders, alpha=0.5, lambda = lambdas, family="gaussian", parallel = TRUE, type.measure = "mae")
CV_Ridge_Gaus.cv3 <- cv.glmnet(formula, DF$murders, alpha=0, lambda = lambdas, family="gaussian", parallel = TRUE, type.measure = "mae")


#In this part we will try to predict the number of murders with the standard(gaussian) regression
#However, because we have count data and Gaussian Regression does not give discrete values we will round the results to the nearest integer

#PLOT CROSS VALIDATION
par(mfrow=c(2,2))
plot(CV_Lasso_Gaus.cv);
title(main="LASSO", line=2)
plot(CV_ElasticNet_Gaus.cv);
title(main="ELASTICNET", line=2)
plot(CV_Ridge_Gaus.cv)
title(main="RIDGE", line=2)


#find estimators according to optimal lambas with different types of measurements
coef(lasso_glmGaus, s = CV_Lasso_Gaus.cv$lambda.min)

#Build the final Poisson Regression
modelGaus <-  glm(murders~., data = DF[,c('persPoverty', 'kidsBornNevrMarr', 'numForeignBorn', 'houseVacant', 'murders')], family = ("gaussian"))
summary(modelPoisson)$coefficients

write.csv(data.frame(summary(modelPoisson)$coefficients),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Homework3/GaussReg.csv")
#=====NEGATIVE BINOMIAL======
#Negative binomial regression is for modeling count variables,
#usually for over-dispersed count outcome variables.

Lasso_glmNB = glmregNB(murders~., data = DF, alpha = 1, lambda = lambdas)
ElasticNet_glmNB = glmregNB(murders~., data = DF, alpha = 0.5)

#plots coefficients- lambdas
par(mfrow = c(2,2))

plot(Lasso_glmNB, xvar="lambda",label=TRUE)
title(main="LASSO", line=2)
plot(ElasticNet_glmNB, xvar="lambda",label=TRUE)
title(main="ELASTIC_NET", line=2)
plot(Ridge_glmNB, xvar="lambda",label=TRUE)
title(main="RIDGE", line=2)

#Cross-validation
Lasso_glmNB_cv = cv.glmregNB(murders~., data = DF, alpha = 1)
ElasticNet_glmNB_cv = cv.glmregNB(murders~., data = DF, alpha = 0.5)

#Plots
par(mfrow=c(2,2))
plot(Lasso_glmNB_cv)
title(main="LASSO", line=2)
plot(ElasticNet_glmNB_cv)
title(main="ELASTIC_NET", line=2)
plot(Ridge_glmNB_cv)
title(main="RIDGE", line=2)
plot(log(Lasso_glmNB_cv$lambda),Lasso_glmNB_cv$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=Lasso_glmNB_cv$name)
points(log(ElasticNet_glmNB_cv$lambda),ElasticNet_glmNB_cv$cvm,pch=19,col="grey")
points(log(Ridge_glmNB_cv$lambda),Ridge_glmNB_cv$cvm,pch=19,col="blue")
legend("topleft",legend=c("alpha= 1","alpha= .5","alpha 0"),pch=19,col=c("red","grey","blue"), cex = 0.55)


#Custom plot (Because we want to see the standard erros for each lamda)
par(mfrow=c(2,2))
plot(log(Lasso_glmNB_cv$lambda), Lasso_glmNB_cv$cv.error, ylab = 'Standard Error of LL values');
title(main="LASSO", line=2)
plot(log(ElasticNet_glmNB_cv$lambda), ElasticNet_glmNB_cv$cv.error,  ylab = 'Standard Error of LL values');
title(main="ELASTIC_NET", line=2)
plot(log(Ridge_glmNB_cv$lambda), Ridge_glmNB_cv$cv.error,  ylab = 'Standard Error of LL values')
title(main="RIDGE", line=2)
dev.off()


# Fit the final model 
# Performance
# predict(Lasso_glmNB_cv, DF, type = "link", s = phd_lasso.cv$lambda.min)
# preds <- predict(phd_models_lasso, phd_mat, type = "response", s = phd_lasso.cv$lambda.min)

modelNB = glm.nb(murders ~., data=DF[,c('persHomeless', 'houseVacant', 'pctNotSpeakEng', 'persPoverty','murders')])

write.csv(data.frame(summary(modelNB)$coefficients),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Homework3/NBReg.csv")
