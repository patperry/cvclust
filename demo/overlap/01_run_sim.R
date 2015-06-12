library(MASS)	

LDA.pick <- function(U_x, U_y){

	result <- rep(NA,10)
	set.seed(1)

	for(i in 1:10){
		Data <- matrix(NA,20000,2)
		ID <- sample(c(1,2),20000,replace = TRUE)
		Data[which(ID==1),] <- mvrnorm(sum(ID==1),c(U_x, U_y), diag(2))
		Data[which(ID==2),] <- mvrnorm(sum(ID==2),c(-U_x, -U_y), diag(2))

		Xtest <- Data[10001:20000,1]
		Ytest <- Data[10001:20000,2]
		Xtrain <- Data[1:10000,1]
		Ytrain <- Data[1:10000,2]
		##----k=1-------------
		Cond <- TRUE
		while(Cond){
			Kmean <- try(kmeans(Ytrain, 1,nstart = 100),silent=T)
			if( class(Kmean)!="try-error" ){Cond <- FALSE}
		}
		Predict.test <- Kmean$center[rep(1,10000),]
		CV1 <- sum((Predict.test-Ytest)^2)/10000
		##----k=2-------------
		Cond <- TRUE
		while(Cond){
			Kmean2 <- try(kmeans(Ytrain, 2,nstart = 100),silent=T)
			if( class(Kmean)!="try-error" ){Cond <- FALSE}
		}
		Center2 = Kmean2$cluster
		LDA2 <- lda(as.matrix(Xtrain),factor(Center2),prior=c(0.5,0.5))
		Predict.test2 <- Kmean2$center[predict(LDA2,as.matrix(Xtest))$class,]
		CV2 <- sum((Predict.test2-Ytest)^2)/10000
		##------------------------------
		RT <- c(CV1,CV2)
		result[i]<-	which(RT == min(RT))
	}
	return(result)
}

##-----------------------------------------------------------
##------------------------------------------------------------
Raw <- matrix(NA,31*31*10,5)
Raw <- as.data.frame(Raw)
names(Raw) <-c("replicate", "U_x", "U_y", "k", "Group")
Raw$replicate <- rep(1:10,31*31)
Raw$Group <- rep(1:(31*31),each=10)

Seq_x <- seq(0,3,0.1)
Seq_y <- seq(0,3,0.1)

G = 0
for(X in Seq_x){
	for(Y in Seq_y){
		G=G+1
		Raw[Raw$Group==G,]$U_x <- rep(X,10)
		Raw[Raw$Group==G,]$U_y <- rep(Y,10)
		Raw[Raw$Group==G,]$k <- LDA.pick(X,Y)
	}
}

write.csv(Raw,file="Raw_result.csv")

#------------------------------------------------------------
#-------------------------------------------------------------