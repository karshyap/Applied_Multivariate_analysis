rm(list=ls())

library(Hotelling)
#library(DescTools)
#library(plotrix)
library(MASS)
setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework2/Problem6")
DTA <- read.csv("hof_new.csv")

## Extract a few offensive statistics (numerical variables).
num_vars <- c("H", "HR", "RBI", "AVG", "SLG", "OBP")
X <- as.matrix(DTA[, num_vars])
X_st <- scale(X, center = TRUE, scale = TRUE)
p<-ncol(X_st)

DTA_st <- data.frame(DTA$HOF, X_st)
#DTA_st<-center(DTA_st, mean=TRUE, center=TRUE)

colnames(DTA_st) <- c("HOF", num_vars)

####Problem-6-a#######
lda_out <- lda(HOF ~ H + HR + RBI + AVG + SLG + OBP, data = DTA_st)
lda_coeff<-lda_out$scaling

## Compute linear discriminant.
ld <- X_st %*% lda_out$scaling

#####problem 6-b########
DTA_st
#H_test=t.test(DTA_st$H,DTA_st$HOF)
pY=DTA_st[DTA_st$HOF=="Y",]
pN=DTA_st[DTA_st$HOF=="N",]

H_test=t.test(pY$H,pN$H)
HR_test=t.test(pY$HR,pN$HR)
RBI_test=t.test(pY$RBI,pN$RBI)
AVG_test=t.test(pY$AVG,pN$AVG)
SLG_test=t.test(pY$SLG,pN$SLG)
OBP_test=t.test(pY$OBP,pN$OBP)

t_test_array=c(H_test$statistic,HR_test$statistic,RBI_test$statistic,AVG_test$statistic,SLG_test$statistic,OBP_test$statistic)


##Problem 6-c######
dta_st=DTA_st

n1<- length(dta_st[dta_st$HOF=='Y', 1])
n2<-length(dta_st[dta_st$HOF=='N', 1])

hot_full<-hotelling.test(dta_st$H+dta_st$HR+dta_st$RBI+dta_st$AVG+dta_st$SLG+dta_st$OBP~dta_st$HOF, data = data.frame(X_st))
hot_red_h<-hotelling.test(dta_st$HR+dta_st$RBI+dta_st$AVG+dta_st$SLG+dta_st$OBP~dta_st$HOF, data = data.frame(X_st))

#F-values for all numerical variables
f_H<-(n1+n2-2-p+1)*(hot_full$stats$statistic-hot_red_h$stats$statistic)/(n1+n2-2+hot_red_h$stats$statistic)

hot_red_hr<-hotelling.test(dta_st$H+dta_st$RBI+dta_st$AVG+dta_st$SLG+dta_st$OBP~dta_st$HOF, data = data.frame(X_st))
f_HR<-(n1+n2-2-p+1)*(hot_full$stats$statistic-hot_red_hr$stats$statistic)/(n1+n2-2+hot_red_hr$stats$statistic)

hot_red_rbi<-hotelling.test(dta_st$H+dta_st$HR+dta_st$AVG+dta_st$SLG+dta_st$OBP~dta_st$HOF, data = data.frame(X_st))
hot_red_avg<-hotelling.test(dta_st$H+dta_st$HR+dta_st$RBI+dta_st$AVG+dta_st$SLG+dta_st$OBP~dta_st$HOF, data = data.frame(X_st))
hot_red_slg<-hotelling.test(dta_st$H+dta_st$HR+dta_st$RBI+dta_st$AVG+dta_st$OBP~dta_st$HOF, data = data.frame(X_st))
hot_red_obp<-hotelling.test(dta_st$H+dta_st$HR+dta_st$RBI+dta_st$AVG+dta_st$SLG~dta_st$HOF, data = data.frame(X_st))

f_rbi<-(n1+n2-2-p+1)*(hot_full$stats$statistic-hot_red_rbi$stats$statistic)/(n1+n2-2+hot_red_rbi$stats$statistic)
f_avg<-(n1+n2-2-p+1)*(hot_full$stats$statistic-hot_red_avg$stats$statistic)/(n1+n2-2+hot_red_avg$stats$statistic)
f_slg<-(n1+n2-2-p+1)*(hot_full$stats$statistic-hot_red_slg$stats$statistic)/(n1+n2-2+hot_red_slg$stats$statistic)
f_obp<-(n1+n2-2-p+1)*(hot_full$stats$statistic-hot_red_obp$stats$statistic)/(n1+n2-2+hot_red_obp$stats$statistic)

f_all=c(f_H,f_HR,f_rbi,f_avg,f_slg,f_obp)
######Problem 6-d######
par(mfrow=c(1,1))

hist(ld[dta_st$HOF=='N'], col='red',xlab = 'Linear Discriminants', main='Comparitive Histigrams of HOF and non-HOF', ylim = c(0,200) )
hist(ld[dta_st$HOF=='Y'], col = 'green', , add=TRUE )
