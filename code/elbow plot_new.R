
library("clusterCrit")
library(MASS)

##---------------------Top plot of figure 1-----------------------------------
set.seed(3)

pdf("Data_example.pdf")
D1 <- mvrnorm(20,c(1.7,3),0.15*diag(2))
plot(D1[,1],D1[,2],ylim=c(0,4.5),xlim=c(0,4.5),col="red",xlab="X1",ylab="X2")

D2 <- mvrnorm(20,c(1,1),0.15*diag(2))
points(D2[,1],D2[,2],ylim=c(0,5),xlim=c(0,5),col="blue")

D3 <- mvrnorm(20,c(3,1.5),0.15*diag(2))
points(D3[,1],D3[,2],ylim=c(0,5),xlim=c(0,5),col="green")

D4 <- mvrnorm(20,c(3.7,3.7),0.1*diag(2))
points(D4[,1],D4[,2],ylim=c(0,5),xlim=c(0,5))
dev.off()


Data <- rbind(D1,D2,D3,D4)

Disp <- rep(NA,10)
for(w in 1:10){
 	cl <- kmeans(Data,w,nstart = 100)
      Disp[w] <- intCriteria(Data,cl$cluster,"trace_w")[[1]]
}
pdf("W_k.pdf")
plot(1:10,Disp,type="b",ylab = "Within-cluster dispersion Wk",xlab="Number of clusters k")
dev.off()

##---------------------Bottom plot of figure 1-----------------------------------
set.seed(1)

pdf("Data_elbow.pdf")
D1 <- mvrnorm(100,c(5,5),3*diag(2))
plot(D1[,1],D1[,2],ylim=c(0,15),xlim=c(0,15),col="red",xlab="X1",ylab="X2")

D2 <- mvrnorm(20,c(5,12),diag(2))
points(D2[,1],D2[,2],ylim=c(0,15),xlim=c(0,15),col="blue")

D3 <- mvrnorm(10,c(9,9),0.5*diag(2))
points(D3[,1],D3[,2],ylim=c(0,15),xlim=c(0,15),col="green")

D4 <- mvrnorm(100,c(12,12),diag(2))
points(D4[,1],D4[,2],ylim=c(0,15),xlim=c(0,15))
dev.off()


Data <- rbind(D1,D2,D3,D4)

Disp <- rep(NA,10)
for(w in 1:10){
 	cl <- kmeans(Data,w,nstart = 100)
      Disp[w] <- intCriteria(Data,cl$cluster,"trace_w")[[1]]
}
pdf("elbow.pdf")
plot(1:10,Disp,type="b",ylab = "Within-cluster dispersion Wk",xlab="Number of clusters k")
dev.off()
