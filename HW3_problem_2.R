rm(list=ls())

setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework3/problem2")

clrscr<-function(){
  for(i in 1:100) {cat("\n")}
}
library(MASS)
dat<-read.csv("stock_prices.csv")
dat_st<-scale(dat, center=TRUE, scale = TRUE)
x_bar<-colMeans(dat)
S<-cov(dat)
R<-cor(dat)

pca<-princomp(dat_st)
pca
summary(pca)
pca1=prcomp(dat_st)


###Verifying the question 2-d###


ee<-eigen(R)
ee_values=ee$values

var_pc<-pca$sdev**2



###Scatter Plots
plot(pca$scores[,1], pca$scores[,2])

#Finding max and min values
pc1_min=which.min(pca$scores[,1])
pc1_max=which.max(pca$scores[,1])
