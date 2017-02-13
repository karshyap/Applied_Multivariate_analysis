rm(list=ls())
setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework2/problem4")
data_read=read.csv("sweat.csv")

####Question 4-a##########
qqnorm(data_read$Sweat, main = "Sweat")
qqline(data_read$Sweat)

qqnorm(data_read$Potassium, main = "Potassium")
qqline(data_read$Potassium)

qqnorm(data_read$Sodium, main = "Sodium")
qqline(data_read$Sodium)

pairs(~Sweat+Sodium+Potassium,data=data_read, 
      main="Scatterplots")

######Question 4-b#######
conf_region_f <- function(n, x_bar, S, alpha) {
  p <- 2
  c2 <- (n - 1) * p * qf(1 - alpha, p, n - p) / (n - p)
  
  ee <- eigen(S)
  lambda <- ee$values
  ee <- ee$vectors
  theta <- acos(abs(ee[1, 1])) * 57.2957795
  if(prod(sign(ee[, 1])) == -1)
    theta <- 180 - theta
  
  ## Draw (1 - alpha)100% confidence region for mean vector.
  xlim <- x_bar[1] + c(-1, 1) * 2 * sqrt(S[1, 1] / n)
  ylim <- x_bar[2] + c(-1, 1) * 2 * sqrt(S[2, 2] / n)
  plot(xlim, ylim, xlab = expression(mu[1]), ylab = expression(mu[2]), asp = 1, 
       xaxt = "n", yaxt = "n", type = "n")
  title(main = paste((1 - alpha) * 100, "% Confidence Region and Intervals", sep = ""))
  draw.ellipse(x_bar[1], x_bar[2], sqrt(c2 * lambda[1] / n), sqrt(c2 * lambda[2] / n), 
               angle = theta, lwd = 2)
  
  ## Simultaneous (1 - alpha)100% confidence intervals for mean components.
  ci_sim_1 <- x_bar[1] + c(-1, 1) * sqrt(c2 * S[1, 1] / n)
  ci_sim_2 <- x_bar[2] + c(-1, 1) * sqrt(c2 * S[2, 2] / n)
  lines(rep(ci_sim_1[1], 2), c(0, ci_sim_2[2]), lwd = 2, lty = 2, col = "red")
  lines(rep(ci_sim_1[2], 2), c(0, ci_sim_2[2]), lwd = 2, lty = 2, col = "red")
  lines(c(0, ci_sim_1[2]), rep(ci_sim_2[1], 2), lwd = 2, lty = 2, col = "red")
  lines(c(0, ci_sim_1[2]), rep(ci_sim_2[2], 2), lwd = 2, lty = 2, col = "red")
  axis(1, at = c(ci_sim_1[1], x_bar[1], ci_sim_1[2]), labels = round(c(ci_sim_1[1], 
                                                                       x_bar[1], ci_sim_1[2]), 1))
  axis(2, at = c(ci_sim_2[1], x_bar[2], ci_sim_2[2]), labels = round(c(ci_sim_2[1], 
                                                                       x_bar[2], ci_sim_2[2]), 1))
  
  ## Bonferroni (1 - alpha)100% confidence intervals for mean components.
  ci_bon_1 <- x_bar[1] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[1, 1] / n)
  ci_bon_2 <- x_bar[2] + c(-1, 1) * qt(1 - alpha / (2 * p), n - 1) * sqrt(S[2, 2] / n)
  lines(rep(ci_bon_1[1], 2), c(0, ci_bon_2[2]), lwd = 2, lty = 3, col = "blue")
  lines(rep(ci_bon_1[2], 2), c(0, ci_bon_2[2]), lwd = 2, lty = 3, col = "blue")
  lines(c(0, ci_bon_1[2]), rep(ci_bon_2[1], 2), lwd = 2, lty = 3, col = "blue")
  lines(c(0, ci_bon_1[2]), rep(ci_bon_2[2], 2), lwd = 2, lty = 3, col = "blue")
  
  text(ci_sim_1[2] + 0.05, ci_bon_2[2], expression(T^2), col = "red", adj = c(0, 0))
  text(ci_sim_1[2] + 0.05, ci_bon_2[2] - 0.05, "Bonferroni", col = "blue", adj = c(0, 0))
}

alpha=0.05

n=length(data_read$Sweat)
p=length(data_read)
S_var=var(data_read)
mean_vec=colMeans(data_read)
lambda=eigen(S_var)
c=sqrt(((n-1)*p/(n-p))*qf((1-alpha), p, n-p))
Half_length= c* sqrt( lambda$values/n)
centre_of_ellipse=mean_vec

############Question 4-c#######

#Selecting the matricies
a1=matrix(c(1,0,0), nrow=3)
a2=matrix(c(0,1,0), nrow=3)
a3=matrix(c(0,0,1), nrow=3)
tsq_conf_1<- t(a1)%*%colMeans(data_read)+c(-1,1)*(sqrt((p*(n-1)/(n-p))*qf(1-alpha, p, n-p))*sqrt(t(a1)%*%matrix(as.numeric(S_var), nrow=3)%*%a1)/sqrt(n))
tsq_conf_2<- t(a2)%*%colMeans(data_read)+c(-1,1)*(sqrt((p*(n-1)/(n-p))*qf(1-alpha, p, n-p))*sqrt(t(a2)%*%matrix(as.numeric(S_var), nrow=3)%*%a2)/sqrt(n))
tsq_conf_3<- t(a3)%*%colMeans(data_read)+c(-1,1)*(sqrt((p*(n-1)/(n-p))*qf(1-alpha, p, n-p))*sqrt(t(a3)%*%matrix(as.numeric(S_var), nrow=3)%*%a3)/sqrt(n))



#sim_2<- t(a2)%*%colMeans(data_read)+c(1,-1)*(sqrt(qf(alpha, p, n-p))*sqrt(t(a2)%*%matrix(as.numeric(S_var), nrow=3)%*%a2)/sqrt(n))
#sim_3<- t(a3)%*%colMeans(data_read)+c(1,-1)*(sqrt(qf(alpha, p, n-p))*sqrt(t(a3)%*%matrix(as.numeric(S_var), nrow=3)%*%a3)/sqrt(n))


###############Question 4-d#####

bonf_intv_1<-mean(data_read[,1])+c(-1,1)%*%(qt((1-alpha)/(2*p), n-1)%*%sqrt(S_var[1,1]/n))
bonf_intv_2<-mean(data_read[,2])+c(-1,1)%*%(qt((1-alpha)/(2*p), n-1)%*%sqrt(S_var[2,2]/n))
bonf_intv_3<-mean(data_read[,3])+c(-1,1)%*%(qt((1-alpha)/(2*p), n-1)%*%sqrt(S_var[3,3]/n))

####i hve some doubts in question 4-d

##Question 4.e##############
##Hotelling's T^2 Test
mu0<-c(4.0,45.0,10.0)
T2<-HotellingsT2Test(data_read,mu=(mu0))
#The null hypothesis is not rejected.

##Question 4.f#############
## Is mu0 in the 95% ellipse?
stat_distance_of_the_mean<-sqrt(n*t(mean_vec-mu0)%*%solve(S_var)%*%(mean_vec-mu0))
stat_distance_from_f_value<- sqrt(qf(1-alpha, p, n-p)*(n-1)*p/(n-p))
#Yes it is inside the confidence ellipsoid

## 4.g#################
## A function to compute T2.
n <- 20
B <- 500

T2_f <- function(X, mu_0) {
  ## The covariance matrices under the null and unrestricted scenarios.
  S <- var(X)
  S_0 <- (t(X) - mu_0) %*% t(t(X) - mu_0) / (n - 1)
  
  # Compute T2 if the sample covariance matrices are non-singular.
  T2 <- NA
  if(det(S) > 0 & det(S_0) > 0) {
    Lambda <- (det(S) / det(S_0)) ^ (n / 2)
    T2 <-  Lambda
  }
  
  return(T2)
}

sim_f <- function(mu, mu_0 = c(4,45,10)) {
  ## Simulate a sample from the multivariate t.
  X <- dta2
  
  ## Observed value of T2.
  T2_0 <- T2_f(X, mu_0)
  T2_0_scaled <- (n - p) / ((n - 1) * p) * T2_0
  
  set.seed(101)
  T2_b <- rep(NA, B)
  X_0 <- t(t(X) - colMeans(X) + mu_0)
  for(b in 1:B) {
    ii <- sample(1:n, replace = TRUE)
    T2_b[b] <- T2_f(X_0[ii, ], mu_0)
  }
  T2_b_scaled <- (n - p) / ((n - 1) * p) * T2_b
  p_value_boot <- mean(T2_b_scaled >= T2_0_scaled, na.rm = TRUE)
  
  return(1-p_value_boot)
}


