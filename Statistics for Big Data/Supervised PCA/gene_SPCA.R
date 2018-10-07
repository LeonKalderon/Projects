#========PACKAGES============
#install.packages('superpc')
library(superpc)
#install.packages("devtools")
library(devtools)

# source("https://bioconductor.org/biocLite.R")
# biocLite(c("GenomicFeatures", "AnnotationDbi"))
# if (!require("BiocManager"))
#   install.packages("BiocManager")
# BiocManager::install("impute", version = "devel")
library(impute)

#===========ETL=========
path = "C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/data.csv"
#Read only the first row
survival_time <-read.table(path, sep = ",", colClasses = "character", nrows = 1)
survival_time[1:2] = NULL
#Parse the strings and extract the number of months that the patient surviced
months = as.numeric(sub("\\ months).*", "", sub(".*\\(", "", unlist(survival_time))))
#Read the rest of the data with the genes values
data = read.csv(path, header = TRUE, sep = ",",strip.white=TRUE,stringsAsFactors = FALSE, skip = 1)[, -2]
dim(data)

#=========DATA EXPLORATION=================

#number of nans IN COLUMNS (OBSERVATIONS)
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


#number of nans per gene (293 genes are nas)
na_countGene <-apply(data, 1, function(y) sum(length(which(is.na(y)))))
na_countGene <- data.frame(na_countGene)
length(na_countGene[na_countGene == 78])

#We observe that 293 genes are empty so we drop them
#the rest of the genes have only few missing values that we will fill with artificial data

#drop rows that all their elements are nans
naGenes <<- NULL
apply(data,1, function(y) if(sum(length(which(is.na(y))))==78)
                                    naGenes <<-c(naGenes,y[1]))
print('Null Rows(Genes)')
length(naGenes)
naGenes <- as.character(naGenes)

data <- data[!data$Systematic.name %in% naGenes,]

#====imputation=======
#http://www.web.stanford.edu/~hastie/Papers/missing.pdf
gnames = data[,1]
data <- apply(data, 2, as.numeric)
data.imputed <- impute.knn(data[,-1], k=5)
DF <- cbind(as.character(gnames), as.table(data.imputed$data))

#=======1rst Selection of important genes(SPEARMAN)=======
#Because we have too many genes (~25k) we do a first selection here
#We keep the genes that are statistically significant 
genes_1step <<- NULL
cor_step1 = function(x){
  res = cor.test(months, as.numeric(x[-1]), method = 'spearman')
  if (res$p.value<0.05){
    genes_1step <<- c(genes_1step, as.character(x[1]))
  }
}

genes_step2 <<- NULL
cor_2step = function(x){
  geneName <- x[1]
  x <- x[-1]
  index1 <- sample(seq(1,78), 43, replace = FALSE, prob = NULL)
  res1 = cor.test(months[index1], as.numeric(x[index1]), method = 'spearman')
  
  index2 <- sample(seq(1,78), 43, replace = FALSE, prob = NULL)
  res2 = cor.test(months[index2], as.numeric(x[index2]), method = 'spearman')
  
  if (res1$p.value<0.05 & res2$p.value<0.05){
    genes_step2 <<- c(genes_step2, as.character(geneName))
  }
}

apply(DF,1, cor_step1)

#create a new dataframe with the genes that are statistically significant
DF_impGenes = DF[DF[, 1] %in% genes_1step, ]

DF_impGenes[, -1] = apply(DF_impGenes[, -1], 1, as.numeric)
DF_impGenes[, -1] = as.matrix(as.data.frame(lapply(DF_impGenes[, -1], as.numeric)))

#=======Train-Test Split=======
# create train and test data objects. censoring.status=1 means the event occurred;
#censoring.status=0 means censored

# set.seed(136)
# train_indecies = sample(seq(2,79), 60, replace = FALSE, prob = NULL)
# x_train = DF_impGenes[, train_indecies]
# x_test = DF_impGenes[, -c(train_indecies,1)]
# 
# y_train = months[(train_indecies-1)]
# y_test = months[-(train_indecies-1)]
# 
# censoring.status.train = rep(1,60)
# censoring.status.test = rep(1,18)
# 
# x_train <- apply(x_train,2 , as.numeric)
# x_test <- apply(x_test,2 , as.numeric)
# 
# 
# data.test = list(x=x_test, y=y_test,  censoring.status = censoring.status.test, featurenames=featurenames)
#=======SPCA=======
#create the object that the superpc library has as parameter
x = DF_impGenes[,-1]
x = apply(x,2,as.numeric)
y = months
featurenames <- as.character(DF_impGenes[, 1])
censoring.status = rep(1,78)
data<-list(x=x, y=y,  censoring.status = censoring.status, featurenames=featurenames)

# train  the model. This step just computes the  scores for each feature
train.obj<- superpc.train(data, type="survival")

#length(as.character(DF[which(train.obj$feature.scores> 0.5),1]))

# cross-validate the model
cv.obj<-superpc.cv(train.obj, data, n.components = 3)

#plot the cross-validation curves. From this plot we see that the 1st 
# principal component is the most significant and the best threshold  is around 0.93
# we see for 20 different thresholds the likelihood ratio of each model for the specific genes
superpc.plotcv(cv.obj)
#the number of genes that we will use to calculate the principal components
length(train.obj$feature[abs(train.obj$feature.scores)>0.92])

#Predict the eigenes
fit.cts<- superpc.predict(train.obj, data, data, threshold=1, n.components=3, prediction.type = "continuous")

#We see that the most important principal component is the first one
model  = superpc.fit.to.outcome(train.obj, data, fit.cts$v.pred)
write.csv(data.frame(model$coeftable),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/finalProject/RegPCs.csv")
model
# Finally, we look for a predictor of survival a small number of
#genes (rather than all 3500 genes). We do this by computing an importance
# score for each equal its correlation with the supervised PC predictor.
# Then we soft threshold the importance scores, and use the shrunken
# scores as gene weights to from a reduced predictor. Cross-validation
# gives us an estimate of the best amount to shrink and an idea of
#how well the shrunken predictor works.
fit.red<- superpc.predict.red(train.obj, data, data, threshold=0.93, n.components = 1)

fit.redcv<- superpc.predict.red.cv(fit.red, cv.obj,  data,  threshold=0.93)

superpc.plotred.lrtest(fit.redcv)

#shrinkage ~0.5 so we will get the 75 most important genes
finalGenes = superpc.listfeatures( data = data, train.obj, fit.red, num.features =  75)
finalGenes
write.csv(data.frame(finalGenes),
          file="C:/Users/User/Desktop/MSc Courses/Statistics for Big Data/finalProject/finalGenes.csv")


# DF_final = DF_impGenes[DF_impGenes[, 1] %in% finalGenes[,3],]
# DF_final = rbind(months, DF_final[,-1])
# DF3 = DF_final[ , order(as.numeric(DF_final[1,]))]
# DF3 = DF3[-1,]
# DF3 = apply(DF3, 2, as.numeric)
# heatmap(DF3)




