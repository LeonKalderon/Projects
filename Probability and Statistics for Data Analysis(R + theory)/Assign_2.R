x<- c(210, 180, 187, 175, 175, 176, 190, 206, 173)
y<- c(201, 175, 184, 170, 168, 169, 183, 199, 168)
d<- x-y

# (A)
#In case of small data sets, a test of significance for normality may lack power to detect the deviation of the variable from normality.
#Even though for sample size n<50  Shapiro-Wilk and  Anderson-Darling may be our best choice.
qqnorm(x);qqline(x, col = 2)
#install.packages('nortest')
library(nortest)
shapiro.test(x)
ad.test(x)

t.test(x,conf.level=0.95)

Tlowerlimit <- mean(x) - qt(0.975,length(x)-1) * sd(x)/sqrt(length(x))
Tupperlimit <- mean(x) + qt(0.975,length(x)-1) * sd(x)/sqrt(length(x))
Tlowerlimit
Tupperlimit

# (B)
t.test(y,conf.level=0.90)

Tlowerlimit <- mean(y) - qt(0.95,length(y)-1) * sd(y)/sqrt(length(y))
Tupperlimit <- mean(y) + qt(0.95,length(y)-1) * sd(y)/sqrt(length(y))
Tlowerlimit
Tupperlimit
# (C)
t.test(d,conf.level=0.95)

Tlowerlimit <- mean(d) - qt(0.975,length(d)-1) * sd(d)/sqrt(length(d))
Tupperlimit <- mean(d) + qt(0.975,length(d)-1) * sd(d)/sqrt(length(d))
Tlowerlimit
Tupperlimit

# (D) We have two dependent samples 
t.test(x,y, paired=T, mu=0,alternative="greater")



#Exercise 5
#install.packages('dplyr')
library('dplyr')
gender <- c(1,2,1,1,2,2,2,1,1,1,1,1,2,2,1,2,1,1,1,1,1,1,1,1,2,1,1,2,2,1)
marketing <- c(99, 89, 32, 44, 45, 25, 47, 86, 97, 94, 55, 61, 94, 55, 66, 60, 83, 67, 46, 52, 29, 54, 25, 90, 33, 77, 79, 97, 100 , 25)
accounting <- c(89, 92, 37, 51, 74, 57, 30, 55, 49, 45, 68, 47, 87, 34, 36, 59, 94, 42, 28, 39, 61, 56, 49, 80, 29, 55, 46, 93, 92, 64)
satisfaction <- c(1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1)

students<-data.frame(gender, marketing, accounting, satisfaction)
# (A)
#95% CI for the percentage - proportion
x <- nrow(filter(students, satisfaction == 1))
n <-nrow(students)
phat <- x/n

Zlowerlimit <- phat - qnorm(0.975,0,1) * sqrt(phat*(1-phat)/n)
Zupperlimit <- phat + qnorm(0.955,0,1) * sqrt(phat*(1-phat)/n)
Zlowerlimit
Zupperlimit

# (B)
#99% CI for the difference in percentages - proportions
xm<-nrow(filter(students, satisfaction == 1, gender == 1))
nmale<-nrow(filter(students, gender == 1))
p_malehat <- xm/nmales

yf <- nrow(filter(students, satisfaction == 1, gender == 2))
nfem<- nrow(filter(students, gender == 2))
p_femalhat <- yf/nfem

Zlowerlimit <- (p_malehat-p_femalhat) - qnorm(0.995,0,1) * sqrt((p_malehat*(1-p_malehat)/nx)+(p_femalhat*(1-p_femalhat)/ny))
Zupperlimit <- (p_malehat-p_femalhat) + qnorm(0.995,0,1) * sqrt((p_malehat*(1-p_malehat)/nx)+(p_femalhat*(1-p_femalhat)/ny))
Zlowerlimit
Zupperlimit

# (C) a=0.05  h0 pmale = pfemale
pw<- (nx*p_malehat+ny*p_femalhat) / (nx+ny)

Z<- (p_malehat-p_femalhat) / sqrt((pw*(1-pw)/nx)+(pw*(1-pw)/ny))
Z
qnorm(0.975,0,1)
#we do not reject H0

# (D)
mark_m <- filter(students, gender == 1)$marketing
mark_f <- filter(students, gender == 2)$marketing

#we have two samples with different variance so we need to calculate the degrees of freedom

df = (var(mark_m)/nmale + var(mark_f)/nfem)^2/ (var(mark_m)^2/(nmale-1))+ (var(mark_f)^2 / (nfem-1))

Zlowerlimit <- mean(mark_m)-mean(mark_f) - qt(0.975, df = df)*sqrt(var(mark_m)/nmale + var(mark_f)/nfem)
Zupperlimit <- mean(mark_m)-mean(mark_f) + qt(0.975, df = df)*sqrt(var(mark_m)/nmale + var(mark_f)/nfem)
Zlowerlimit
Zupperlimit

# (E)

t = (mean(mark_m) - mean(mark_f)) / sqrt((var(mark_m)/nmale + var(mark_f)/nfem))

# t= -0.13  qt(0.025, df = df )< t < qt(0.975, df = df )
#So we do NOT reject H0 that sais the two means are equal.

# (F)
phat = nrow(filter(students, accounting >= 85)) / nrow(students)
p0 <- 0.095
#hypothesis testing for phat > 0.095

Z= (phat-p0)/ sqrt(p0*(1-p0)/nrow(students))

#one-tail test
qnorm(0.95)

# Z > Z(1-a) so we reject the Null hypothesis that the two percentages are the same