rm(list=ls())
library(MASS)
setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework2/problem3")
data_read=read.csv("used_cars.csv")

bc_age <- boxcox(data_read$Age ~ 1)
title("BC-plot For Age column")

bc_price <- boxcox(data_read$Price ~ 1)
title("BC-plot For Price column")

lam_max_age=bc_age$x[which.max(bc_age$y)]
lam_max_price=bc_price$x[which.max(bc_price$y)]

#dta_tr <- (dta ^ 0.25 - 1) / 0.25
data_tr_age= ((data_read$Age^lam_max_age)-1)/lam_max_age
data_tr_price= ((data_read$Price^lam_max_price)-1)/lam_max_price
data_tr=data.frame(data_tr_age,data_tr_price)
colnames(data_tr)=c("Age","Price")

qqnorm(data_tr[, 1], main = "Age")
qqline(data_tr[, 1])

qqnorm(data_tr[, 2], main = "Price")
qqline(data_tr[, 2])

#######For question 3-c#######
n <- nrow(data_read)
p <- 2
lambda_seq <- seq(from = -3, to = 3, length = 100)
obj <- matrix(NA, nrow = 100, ncol = 100)
csld <- colSums(log(data_read))
for(i in 1:100) {
  for(j in 1:100) {
    X_l <- data_read
    lambda <- lambda_seq[c(i, j)]
    
    for(k in 1:2) {
      if(lambda[k] != 0) {
        X_l[, k] <- (X_l[, k] ^ lambda[k] - 1) / lambda[k]
      } else {
        X_l[, k] <- log(X_l[, k])
      }
    }
    S <- var(X_l)
    
    obj[i, j] <- -(n / 2) * log(det(S)) + (lambda - 1) %*% csld
  }
}


obj_df=data.frame(obj)
p_min=max(obj)
min_x=0
min_y=0
for(i in 1:length(obj_df)){
  for(j in 1:length(obj_df$X1))
  {
    if(obj_df[i,j]==p_min){
      min_x=i
      min_y=j
    }
  }
}

min_mul_lam1=lambda_seq[min_x]
min_mul_lam2=lambda_seq[min_y]

par(mfrow = c(1, 1))
contour(lambda_seq, lambda_seq, obj, xlab = expression(lambda[1]), 
        ylab = expression(lambda[2]))
points(min_mul_lam1, min_mul_lam2, pch = 20, cex = 2, col = "red")
text(min_mul_lam1+0.5, min_mul_lam2+0.5, expression(paste(hat(lambda), "' = [1.25, 0.04]", sep = "")), lwd = 2)






#min_x=which(obj_df

