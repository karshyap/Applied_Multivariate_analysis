#####Question 5-a#####

rm(list=ls())

#library(Hotelling)
#library(DescTools)
#library(plotrix)

setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework2/problem5")
dta=read.csv("peanut.csv")

#colnames(dta) <- c("Factor_1", "Factor_2", "X_1", "X_2", "X_3")
#dta$Factor_1=factor(dta$Factor_1)
#dta$Factor_2=factor(dta$Factor_2)



attach(dta)

#p1=summary(manova(as.matrix(dta[, 3:5]) ~ Factor_1+Factor_2+Factor_1*Factor_2),test = "Wilks")

###Problem 5-b######

n <- 2
p <- 3
g <- 2 
b <- 3
dta3=dta
attach(dta3)

## Summary statistics.
x_bar <- colMeans(dta3[, 3:5])
x_bar_lk <- rbind(colMeans(dta3[Location == 1 & Variety == 5, 3:5]),
                  colMeans(dta3[Location == 2 & Variety == 5, 3:5]),
                  colMeans(dta3[Location == 1 & Variety == 6, 3:5]),
                  colMeans(dta3[Location == 2 & Variety == 6, 3:5]),
                  colMeans(dta3[Location == 1 & Variety == 8, 3:5]),
                  colMeans(dta3[Location == 2 & Variety == 8, 3:5]))



x_bar_l_dot <- rbind(colMeans(dta3[Location == 1, 3:5]), colMeans(dta3[Location == 2, 3:5]))
x_bar_dot_k <- rbind(colMeans(dta3[Variety == 5, 3:5]), colMeans(dta3[Variety == 6, 3:5]),colMeans(dta3[Variety == 8, 3:5]))



## Components for MANOVA.
SSP_cor <- SSP_fac_1 <- SSP_fac_2 <- SSP_int <- SSP_res <- matrix(0, nrow = p, ncol = p)
for(l in 1:g) {
  SSP_fac_1 <- SSP_fac_1 + b * n * t(x_bar_l_dot[l, , drop = FALSE] - x_bar) %*% 
    (x_bar_l_dot[l, , drop = FALSE] - x_bar)
}
for(k in 1:b){
  SSP_fac_2 <- SSP_fac_2 + g * n * t(x_bar_dot_k[k, , drop = FALSE] - x_bar) %*% 
    (x_bar_dot_k[k, , drop = FALSE] - x_bar)
}
for(k in 1:b) {
  for(l in 1:g){
    SSP_int <- SSP_int + n * t(x_bar_lk[(k - 1) * 2 + l, , drop = FALSE] -  x_bar_l_dot[l, , drop = FALSE] - x_bar_dot_k[k, , drop = FALSE] + x_bar) %*% 
      (x_bar_lk[(k - 1) * 2 + l, , drop = FALSE] - x_bar_l_dot[l, , drop = FALSE] - x_bar_dot_k[k, , drop = FALSE] + x_bar)
  }
}
for(l in 1:g) {
  for(k in 1:b) {
    for(r in 1:n){
      SSP_res <- SSP_res + t(as.matrix(dta3[(l - 1) * 3 * n + (k - 1) * n + r, 3:5]) - x_bar_lk[(l - 1) * 3 + k, , drop = FALSE]) %*% 
        (as.matrix(dta3[(l - 1) * 3 * n + (k - 1) * n + r, 3:5]) - x_bar_lk[(l - 1) * 3 + k, , drop = FALSE])
      SSP_cor <- SSP_cor + t(as.matrix(dta3[(l - 1) * 3 * n + (k - 1) * n + r, 3:5]) - x_bar) %*% (as.matrix(dta3[(l - 1) * 3 * n + (k - 1) * n + r, 3:5]) - x_bar)
    }
  }
}

lam_fact1=det(SSP_res)/(det(SSP_res+SSP_fac_1))
lam_fact2=det(SSP_res)/(det(SSP_res+SSP_fac_2))
lam_int=det(SSP_res)/(det(SSP_res+SSP_int))

fac1_brac=(((g*b*(n-1))-p+1)/(abs((g-1)-p)+1))*((1-lam_fact1)/lam_fact1)
fac2_brac=(((g*b*(n-1)-p+1))/(abs((b-1)-p)+1))*((1-lam_fact2)/lam_fact2)
int_brac=(((g*b*(n-1)-p+1))/(abs(((b-1)*(g-1))-p)+1))*((1-lam_int)/lam_int)

vfac1_1=abs((g-1)-p)+1
vfac1_2=((g*b*(n-1)-p+1))
#cat("grigor")

vfac2_1=abs((b-1)-p)+1
vfac2_2=((g*b*(n-1)-p+1))


vint_1=abs(((b-1)*(g-1))-p)+1
vint_2=((g*b*(n-1)-p+1))

#cat("grigor")
alpha=0.05
F_value_fact1=qf(alpha,vfac1_1,vfac1_2)
F_value_fact2=qf(alpha,vfac2_1,vfac2_2)
F_value_int=qf(alpha,vint_1,vint_2)






