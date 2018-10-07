#EXERCISE 6
#1.
m<-100	
s<-15	
1-pnorm(125, m, s)	 
pnorm(110,m,s)-pnorm(90,m,s)
#2.
N<-15		# number of trials
p<-0.2	# probability of success
1-pbinom(2, N, p)	



#EXERCISE 7
#We set seed so we can ensure that we can reproduce the same 'random' results every time we run the code
set.seed(1)
n1<-100
n2<-10000

#Asymmetric continuous distribution(gamma distribution)
#These are the parametres that we will use.
alpha = 3
beta = 12
#generate samples
obs1 <- rgamma(n1, alpha, rate = 1/beta)
obs2 <- rgamma(n2, alpha, rate = 1/beta)

#plot the sample of 100 observations and make a comparison with the true distribution
#we can easily observe that there are discrepancies due to the small sample
hist(obs1, prob=TRUE, ylim=c(0,0.03), breaks=20, main = 'Histogram of 100 observations', sub = 'Gamma', ylab = 'f(x)', xlab='x')
curve(dgamma(x, alpha, rate = 1/beta), add=TRUE, col='red', lwd=2)

#plot the sample of 10000 observations and make a comparison with the true distribution
#this time the discrepancies have been eliminated because the number of observations is large. 
#The true density fits almost perfectly to the histogram
hist(obs2, prob=TRUE, ylim=c(0,.03), breaks=50, main = 'Histogram of 10000 observations', sub = 'Gamma', ylab='f(x)', xlab='x')
curve(dgamma(x, alpha, rate = 1/beta), add=TRUE, col='black', lwd=2)

#calculate the descriptive statistics for the two samples and add two extra columns with the IQR and standard deviation
descriptives_1 <- round(summary(obs1), 2)
descriptives_1['IQR'] <- round(IQR(obs1), 2)
descriptives_1['sd'] <- round(sd(obs1), 2)
descriptives_2 <- round(summary(obs2), 2)
descriptives_2['IQR'] <- round(IQR(obs2), 2)
descriptives_2['sd'] <- round(sd(obs2), 2)

#calculate the descriptive statistics of the true distribution
avg <- round(alpha*beta, 2)
sd <- round(sqrt(alpha*beta^2), 2)
median <- round(qgamma(0.5, alpha, rate=1/beta), 2)
IQR <-round(qgamma(0.75, alpha, rate = 1/beta) - qgamma(0.25, alpha, rate = 1/beta), 2)

#we create a table with all the descriptives statistics so we can compare them side by side
#we notice that that as the sample becomes larger the descriptive values tend to the theoretic values.
#this is very logical because the random factor is eliminated
summary_descriptives <-rbind(descriptives_1[c('Median','Mean','IQR','sd')],
                             descriptives_2[c('Median','Mean','IQR','sd')], c(median,avg,IQR,sd))

rownames(summary_descriptives) <- c('100 Observ', '10,000 Observ', 'Theoretic Values')
summary_descriptives
#c we calculate the proportion of the sample data that exist between the mean and the median.
# one more time the big sample is very close to the theoretical value.
length(obs1[obs1>= min(descriptives_1['Median'],descriptives_1['Mean']) & obs1<=max(descriptives_1['Median'],descriptives_1['Mean'])])/n1
length(obs2[obs2>= min(descriptives_2['Median'],descriptives_2['Mean']) & obs2<=max(descriptives_2['Median'],descriptives_2['Mean'])])/n2
pgamma(max(avg,median), alpha , rate = 1/beta)-pgamma(min(median,avg), alpha, rate = 1/beta)

#d
#true distribution 1% quantile
qgamma(0.01 , alpha, rate = 1/beta)
#100 observ 1% quantile
quantile(obs1, 0.01)
#10000 observ 1% quantile
quantile(obs2, 0.01)

#e
#theoritical 99% quantile
qgamma(0.99 , alpha, rate = 1/beta)
#100 observations sample 99% quantile
quantile(obs1, 0.99)
#10000 observations 99% quantile
quantile(obs2, 0.99)

#f 
#As we have mentioned above,the big sample of 10,000 observations can approach very well the theoretical values.
#On the other hand, there are some discrepancies between the theoretical values and the values of a random sample of 100 observations.


#Asymmetric discrete distribution(Poisson distribution)
#These are the parametres that we will use.
lam <- 1.2

#generate samples
sample1 <- rpois(n1,lambda = lam)
sample2 <- rpois(n2, lambda = lam)

#Draw the true poisson distribution
x<-0:12
p<-dpois(x, lam)
barplot(p,names=x, ylim=c(0,0.5), xlab="x", ylab="P(X=x)",
        main=expression(paste("Poisson distribution with ",lambda," = 3")), sub = 'Poisson')

#plot the sample of 100 observations
#we can easily observe that there are discrepancies due to the small sample
barplot(table(sample1)/n1, ylim = c(0,0.5), xlab='x', ylab = 'P(X=x)', main = 'Sample of 100 observations', sub = 'Poisson')

#plot the sample of 10000 observations
barplot(table(sample2)/n2, ylim=c(0,0.5), main = 'Histogram of 10000 observations', ylab='P(X=x)', xlab='x', sub='Poisson')

#calculate the descriptive statistics for the two samples and add two extra columns with the IQR and standard deviation
descriptives_1 <- round(summary(sample1), 2)
descriptives_1['IQR'] <- round(IQR(sample1), 2)
descriptives_1['sd'] <- round(sd(sample1), 2)
descriptives_2 <- round(summary(sample2), 2)
descriptives_2['IQR'] <- round(IQR(sample2), 2)
descriptives_2['sd'] <- round(sd(sample2), 2)

#calculate the descriptive statistics of the true distribution
avg <- round(lam, 2)
sd <- round(sqrt(lam), 2)
median <- round(qpois(0.5, lambda = lam), 2)
IQR <-round(qpois(0.75, lambda = lam) - qpois(0.25, lambda =lam), 2)

#we create a table with all the descriptives statistics so we can compare them side by side
#we notice that that as the sample becomes so large the descriptive values tend to the theoretic values.
#this is very logical because the random factor is eliminated
summary_descriptives <-rbind(descriptives_1[c('Median','Mean','IQR','sd')],
                             descriptives_2[c('Median','Mean','IQR','sd')], c(median,avg,IQR,sd))

rownames(summary_descriptives) <- c('100 Observ', '10,000 Observ', 'Theoretic Values')
summary_descriptives

#c we calculate the proportion of the sample data that exist between the mean and the median.
#because we are searching for the close interval between mean and median, P(X=1) will be included in that proportion
length(sample1[sample1 >= min(descriptives_1['Median'],descriptives_1['Mean']) & sample1 <= max(descriptives_1['Median'],descriptives_1['Mean'])])/n1
length(sample2[sample2 >= min(descriptives_2['Median'],descriptives_2['Mean']) & sample2<= max(descriptives_2['Median'],descriptives_2['Mean'])])/n2
ppois(max(median,avg), lambda = lam, lower.tail = TRUE, log.p = FALSE)-ppois(min(median,avg)-0.01, lambda = lam,lower.tail = TRUE, log.p = FALSE)

#d
qpois(0.01 , lambda = lam)
quantile(sample1, 0.01)
quantile(sample2, 0.01)
#e
qpois(0.99 , lambda = lam)
quantile(sample1, 0.99)
quantile(sample2, 0.99)

#f 
#One more time, the large sample of 10,000 observations can approach very well the theoretical values.
#On the other hand, there are some discrepancies between the theoretical values and the values of a random sample of 100 observations.
#There are more details on the comments of each question.

