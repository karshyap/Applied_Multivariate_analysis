rm(list=ls())
library(MASS)
setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework3/problem1")

kappa <- seq(from = 0, to = 1, by = 0.01)
DTA_st=read.csv("hof_new.csv")

#####part-a####
lda_out <- lda(HOF ~ H + HR + RBI + AVG + SLG + OBP, data = DTA_st, CV = TRUE)
qda_out <- qda(HOF ~ H + HR + RBI + AVG + SLG + OBP, data = DTA_st, CV = TRUE)
bacc_lda <- sens_lda <- spec_lda <- ppv_lda <- npv_lda <- numeric(length(kappa))
bacc_qda <- sens_qda <- spec_qda <- ppv_qda <- npv_qda <- numeric(length(kappa))
sens_kappa_lda=spec_kappa_lda=sens_kappa_qda=spec_kappa_qda=numeric(length(kappa))

for(i in 1:length(kappa)) { 
  cls_lda <- ifelse(lda_out$posterior[, 2] > kappa[i], "Y", "N") 
  class_kappa_lda=lda_out$posterior[,2]>kappa[i]
  class_kappa_qda=qda_out$posterior[,2]>kappa[i]
  
  
  sens_lda[i] <- mean(lda_out$posterior[DTA_st$HOF == "Y", 2] > kappa[i])
  spec_lda[i] <- mean(lda_out$posterior[DTA_st$HOF == "N", 1] > (1-kappa[i]))
  cls_qda <- ifelse(qda_out$posterior[, 2] > kappa[i], "Y", "N") 
  sens_qda[i] <- mean(qda_out$posterior[DTA_st$HOF == "Y", 2] > kappa[i])
  spec_qda[i] <- mean(qda_out$posterior[DTA_st$HOF == "N", 1] > (1-kappa[i]))
  
  sens_kappa_lda[i]=mean(class_kappa_lda[DTA_st$HOF=="Y"]==TRUE)
  spec_kappa_lda[i]=mean(class_kappa_lda[DTA_st$HOF=="N"]==FALSE)
  
  sens_kappa_qda[i]=mean(class_kappa_qda[DTA_st$HOF=="Y"]==TRUE)
  spec_kappa_qda[i]=mean(class_kappa_qda[DTA_st$HOF=="N"]==FALSE)
  
  
  ppv_lda[i]=mean(DTA_st$HOF[class_kappa_lda==TRUE]=="Y")
  npv_lda[i]=mean(DTA_st$HOF[class_kappa_lda==FALSE]=="N")
  ppv_qda[i]=mean(DTA_st$HOF[class_kappa_qda==TRUE]=="Y")
  npv_qda[i]=mean(DTA_st$HOF[class_kappa_qda==FALSE]=="N")
  
  
  

}


bal_accuracy_lda=(sens_lda+3*spec_lda)/4
bal_accuracy_qda=(sens_qda+3*spec_qda)/4
max_lda_index=match(c(max(bal_accuracy_lda)),bal_accuracy_lda)
max_qda_index=match(c(max(bal_accuracy_qda)),bal_accuracy_qda)
bal_accuracy=bal_accuracy_lda


plot(kappa,bal_accuracy,ylim=c(0.85,0.96),type = "l",col="blue",lwd=2)
lines(kappa,bal_accuracy_qda,ylim=c(0.85,0.96),type = "l",col="green",lwd=2)
points(kappa[max_lda_index],bal_accuracy_lda[max_lda_index],pch=19,col="red")
points(kappa[max_qda_index],bal_accuracy_qda[max_qda_index],pch=19,col="red")

grid()

####part-b#####

kappa_max_index_lda=kappa[max_lda_index]
kappa_max_index_qda=kappa[max_qda_index]

sens_max_index_lda=sens_kappa_lda[max_lda_index]
sens_max_index_qda=sens_kappa_qda[max_qda_index]

spec_max_index_lda=spec_kappa_lda[max_lda_index]
spec_max_index_qda=spec_kappa_qda[max_qda_index]

ppv_max_index_lda=ppv_lda[max_lda_index]
ppv_max_index_qda=ppv_qda[max_qda_index]

npv_max_index_lda=npv_lda[max_lda_index]
npv_max_index_qda=npv_qda[max_qda_index]

bac_max_index_lda=bal_accuracy_lda[max_lda_index]
bac_max_index_qda=bal_accuracy_qda[max_qda_index]






