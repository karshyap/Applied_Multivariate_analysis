rm(list=ls())
setwd("/home/kashyap/Desktop/TAMU_first_semester/Applied_multivariate_analysis/Homeworks/Homework4")
clrscr<-function(){
  for(i in 1:100){
    cat("\n")
  }
}

dta=read.csv("cereal.csv")

hc_complete <- hclust(dist(dta[,2:9]), "complete")
plot(hc_complete,labels = dta[,1])
plot(hc_complete$height)
grid()

km<-kmeans(dta[,2:9],centers = 3, algorithm = "MacQueen")

color<-character(length = nrow(dta))
for(i in 1:nrow(dta)){
  if(km$cluster[i]==1){
    color[i]="green"
  }
  if(km$cluster[i]==2){
    color[i]="blue"
  }
  if(km$cluster[i]==3){
    color[i]="red"
  }
}

##PCA
pca<-princomp(dta[,2:9],cor = TRUE) 
plot(pca$scores[,1], pca$scores[,2],col=color,pch=19)
grid()




