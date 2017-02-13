rm(list = ls())

setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework4")


X <- as.matrix(read.table("T12-8.DAT", header = FALSE))
pot_type <- c("A", "B", "C", "D")
pot_site <- paste("P", 0:6, sep = "_")
rownames(X) <- pot_site
colnames(X) <- pot_type

###part-a##

mds=cmdscale(dist(X),k=2)
plot(mds,pch=19)
text(mds,row.names(X))
grid()


##part-b###
biplot(princomp(X))
grid()