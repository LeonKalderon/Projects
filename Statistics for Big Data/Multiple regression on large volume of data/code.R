#========================================
#=========STATISTICS FOR BIG DATA========
#=============HOMEWORK-2=================
#install.packages('data.table')
library(data.table)
#install.packages('ffbase')
require(ffbase)
#install.packages('biglm')
require(biglm)
library(cluster)
# read our data
#we use fread() because we can store all the data in our memory(if we could not we would use the ff libary)
#we select to read only the features that we will need in our analysis to minimize workload
colsToKeep <- c('ArrDelay', 'Distance',  'DepDelay' , 'DepTime', 'Month', 'DayOfWeek')
DATA <- fread('C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/2005.csv', 
              header = 'auto', select=colsToKeep, sep = ',') 

#=========================================================
#===========Naive ETL approach to prepare our DF==========
#=========================================================

#delete rows with NAs
DATA <- na.omit(DATA)
#convert values to numerical values
DATA <- apply(DATA, 2, as.numeric)
DATA <- as.data.table(DATA)
#Encode Months and DayofWeek to Factors
DATA$MonthF <- as.character(DATA$Month)
#We encode the days of the week to weekends and not-weekends
DATA$Weekend <- as.character(DATA$DayOfWeek > 4)
#drop the columns that we have encoded in new columns
DATA$Month <- NULL
DATA$DayOfWeek <- NULL

#==================================================
#=============BUILD-IN LM WITH ALL POINTS==========
#==================================================

#R'S BUILD-IN MULTIPLE REGRESSION FUNCTION
t1 <- Sys.time();
# On the 6th column we have our dependent variable (ArrDelay)
#The other 5 columns are our independent variables
formula <- unlist(DATA[,1]) ~ .
fit = lm(formula, data = DATA[,2:6])
t2 = Sys.time();
#execution time
print(t2-t1)
summary(fit) # show results

#write the results to a csv file 
write.csv(data.frame(summary(fit)$coefficients),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/BuildInLm.csv")

#==================================================
#===Random Sample of 100,000 points and 10,000=====
#==================================================

#Random Sample
n <- dim(DATA)[1]
s1<- sample(1:n, 100000,replace=FALSE)
s2<- sample(1:n, 10000,replace=FALSE)
#Execute LM for 100k points
t1 <- Sys.time();
formula <- unlist(DATA[s1,1]) ~ .
fit_RandSample1 = lm(formula, data = DATA[s1, 2:6])
t2 = Sys.time();
print(t2-t1)

#Relative difference between 'true' betas and random's sample beta
rel_diff_coeff1 <- (abs(summary(fit)$coefficient[,1] - summary(fit_RandSample1)$coefficient[,1]))/abs(summary(fit)$coefficient[,1])

write.csv(data.frame(cbind(summary(fit_RandSample1)$coefficients, rel_diff_coeff1)),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Sample100k.csv")

#Execute LM for 10k points
t1 <- Sys.time();
formula <- unlist(DATA[s2,1]) ~ .
fit_RandSample2 = lm(formula, data = DATA[s2,2:6])
t2 = Sys.time();
print(t2-t1)

rel_diff_coeff2 <- (abs(summary(fit)$coefficient[,1] - summary(fit_RandSample2)$coefficient[,1]))/abs(summary(fit)$coefficient[,1])

write.csv(data.frame(cbind(summary(fit_RandSample2)$coefficients, rel_diff_coeff2)),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Sample10k.csv")

#==================================================
#===Random Samples of 10,000 points 100 times======
#===and combine the estimators by averaging them===
#==================================================

#create a dataframe to store betas and p-vals of each subsample
#colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
#row.names = c("Intercept", "Month", "DayOfWeek", 'Distance', 'DepDelay', 'DepTime')

betas = data.table()
p_vals = data.table()

#Run regression for 100 subsamples and store the results into betas and p_vals
t1 = Sys.time()
for (i in 1:100){
  t<-sample(1:n,10000,replace=FALSE)
  formula <- unlist(DATA[t,1]) ~ .
  fit_sample <- lm(formula, data = DATA[t,2:6])
  betas <- rbind(betas,t(summary(fit_sample)$coefficient[,1]))
  p_vals <- rbind(p_vals,t(summary(fit_sample)$coefficient[,4]))
}
t2 = Sys.time()
print(t2-t1)

#combine the 100 estimates and find their mean
mean_betas <- apply(betas,2,mean)
mean_pvals <- apply(p_vals,2,mean)
#calculate the relative difference of the coefficients from the true ones
rel_diff_coeff3 <- (abs(summary(fit)$coefficient[,1] - mean_betas))/abs(summary(fit)$coefficient[,1])
sd_betas <- apply(betas,2, sd)

#stack results to a dataframe 
Results <- cbind(mean_betas, sd_betas, mean_pvals, rel_diff_coeff3)
#write to a csv
write.csv(data.frame(Results),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/Combined.csv")


#==================================================
#===========AGGREGATE DATA POINTS =================
#==================================================
#We order the DATA by ArrDelay (Y) and then we group the data into equal buckets
#we find the mean value of Y (ArrDelay) and Xs for each bucket
#then we run lm on the aggregated data
#~~~This is a very naive approach of the problem! In reality, we should calculate Mahalanobis or Euclidean
#distance between Xs (independent variables) and put on the same bucket the points that are closer each other
#However, this approach is very demanding computationally.

#Create a new df with the data ordered by Y
# Ans <- DATA[order(DATA$ArrDelay),1:4]
# n <- dim(Ans)[1]
# 
# Agr = data.table()
# #create buckets of 1000 datapoints
# t1 = Sys.time()
# k = 1
# for (i in seq(1000, n, by=1000)){
#   i=1
#   #find the mean of each bucket and added into a new dataframe Agr
#   row = apply(Ans[k:i,],2, mean)
#   Agr = rbind(Agr, t(row))
#   k=i
# }
# 
# Yagr = unlist(Agr$ArrDelay)
# fit_Aggr = lm(Yagr ~ ., data = unlist(Agr[,2]))
# summary(fit_Aggr)
# t2 = Sys.time()
# print(t2-t1)
# rel_diff_coeff4 <- (abs(summary(fit)$coefficient[,1] - summary(fit_Aggr)$coefficient[,1]))/abs(summary(fit)$coefficient[,1])
# 
# write.csv(data.frame(cbind(summary(fit_Aggr)$coefficients, rel_diff_coeff4)),
#           file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/AggrPoints.csv")

#==================================================
#================AGGREGATION WITH CLARA============
#==================================================
# CLARA: Clustering for large applications
#It works by clustering a sample from the dataset and then assigns all objects in the dataset to these clusters
#CLARA draws a small sample from the data set and applies the clustering algorithm to generate an optimal set
#of centroids for the sample
t1 <- Sys.time()
clarax <- clara(DATA, 10, samples=10, sampsize = 100, metric = c("euclidean"), correct.d=TRUE)
DATA_Agr <- clarax$medoids
t2 <- Sys.time()
print(t2-t1)
fit_CLARA = lm(unlist(DATA_Agr[,1]) ~ ., data = as.data.frame(DATA_Agr[,2:4]))
summary(fit_CLARA)


rel_diff_coeff5 <- (abs(summary(fit)$coefficient[1:4,1] - summary(fit_CLARA)$coefficient[,1]))/
                                                        abs(summary(fit)$coefficient[1:4,1])

write.csv(data.frame(cbind(summary(fit_CLARA)$coefficients[], rel_diff_coeff5)),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/CLARA.csv")

#==================================================
#================BIGLM PACKAGE=====================
#==================================================
#Biglm is a package that can run regression on data that is not possible to fit into memory.
#Also it has the function update() that use sufficient statistics to update
#the estimators without running the whole process all along. 
t1 = Sys.time()
formula = ArrDelay~Distance+DepDelay+DepTime+MonthF+Weekend

#Run biglm on the first 2 million points
fit_bigLM <- biglm(formula, data = DATA)
t2 = Sys.time()
print(t2-t1)
a = summary(fit_bigLM)
p(fit_bigLM)

#we use update() just for the purposes of the excercise 
#chunk1<-DATA[2000000:4000000,]
#fit_bigLM <- update(fit_bigLM, chunk2)
rel_diff_coeff6 <- (abs(summary(fit)$coefficient[,1] - coef(fit_bigLM)))
                                      /abs(summary(fit)$coefficient[,1])

write.csv(data.frame(cbind(coef(fit_bigLM), rel_diff_coeff5)),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/BigLM.csv")

