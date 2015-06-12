
Raw <- read.csv("Raw_result.csv")

Number <- rep(NA,30*30)
X_ax <- rep(NA,30*30)
Y_ax <- rep(NA,30*30)

for(i in unique(Raw$Group) ){
	X_ax[i] <- unique(Raw[Raw$Group==i,]$U_x)
	Y_ax[i] <- unique(Raw[Raw$Group==i,]$U_y)
	Number[i] <-  sum(Raw[Raw$Group==i,]$k == 2)
}

library(plotrix)

pdf("color_plot.pdf")

plot(X_ax,Y_ax,col=color.scale(-Number,c(2,1,0),c(0,2,4),c(3,5,6)),main="Red presents high value",
pch=16,cex=2,xlab="U_x", ylab="U_y")
##-----------------------------------------------

YtoX <- function(U_y){
	Num <- 2*dnorm(U_y)+2*U_y*pnorm(U_y)-U_y
	NM <- (Num/U_y +2)/4
	if(NM<1){
		return(qnorm(NM))
	}else{
		return(NA)
	}
}
##-----------------------------------
Y_vector <- seq(0.437,3,0.01)
X_vector <- rep(NA,257)

for(j in 1:257){
	X_vector[j] <- YtoX(Y_vector[j])
}


lines(X_vector,Y_vector, lwd=3) 

dev.off()
