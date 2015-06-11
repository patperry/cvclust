# demo/bench/settings.R
#
# Depends:
#   library("MASS") # mvrnorm
#


setting1 <- function()
{
    nobs <- 200
    x <- matrix(NA, nobs, 10)
    for(s in 1:10){
        x[,s] <- runif(nobs, 0, 1)
    }

    ngroup <- 1
    group <- as.factor(rep(1, nobs))
    centers <- matrix(rep(0.5, 10), byrow=TRUE, nrow=1)

    mean <- centers[group,]

    list(x=x, mean=mean, ngroup=ngroup, group=group, centers=centers)
}


location <- function(M.data,M.center)
{
   if(ncol(M.data)!=ncol(M.center)){return("dimention don't match")}
   center<-rep(NA,nrow(M.data))
   K <- nrow(M.center)
   for (i in 1:length(center)){
     dist <- rep(NA,K)
     for(j in 1:K){
	 dist[j]<- sqrt(sum((M.center[j,]-M.data[i,])^2))
     }
     center[i]<- which(dist == min(dist))
   }
   return(center)
}


Data2center <- function(number = 50)
{
	u1 = c(1,0.0,0.0,1)
	u2 = c(1,3.5,3.5,1)
    Centers <- matrix(c(1,1,0,3.5,0,3.5,1,1),2,4)

	condition <- TRUE
	while(condition){
	      obs1 <- mvrnorm(n = number, u1, diag(4))
		obs2 <- mvrnorm(n = number, u2, diag(4))

		if(sum(location(obs1,Centers)==1)==number
           & sum(location(obs2,Centers)==2)==number){
			condition <- FALSE
		}
	}

	Data <- rbind(obs1,obs2)
	Mu <- rbind(matrix(u1,nrow=number,ncol=4,byrow=T),
                matrix(u2,nrow=number,ncol=4,byrow=T))
	ID <- sample(1:nrow(Data),nrow(Data),replace = FALSE)
	Data <- Data[ID,]

	Mean <- Mu[ID,]
	list(x = Data, mean = Mean)
}


setting2 <- function()
{
    Data2center(50)
}


Data4center <- function(di = 10, dd = 1.9){ #Another pair is di=4 and dd=5
	condition <- TRUE
	while(condition){
		del = 1
		if(di == 100){
			nn=sample(c(100,150),size=4,replace=T)
		}else{
			nn=sample(c(25,50),size=4,replace=T)
		}
		cl=c(rep(1,nn[1]),rep(2,nn[2]),rep(3,nn[3]),rep(4,nn[4]))

		c1=dd*rnorm(di)
		U1=matrix(c1,nrow=nn[1],ncol=di,byrow=T)
		x1=(matrix(c1,nrow=nn[1],ncol=di,byrow=T)
            + matrix(rnorm(nn[1]*di),ncol=di))
		c2=dd*rnorm(di)
		U2=matrix(c2,nrow=nn[2],ncol=di,byrow=T)
		x2=(matrix(c2,nrow=nn[2],ncol=di,byrow=T)
            + matrix(rnorm(nn[2]*di),ncol=di))
		c3=dd*rnorm(di)
		U3=matrix(c3,nrow=nn[3],ncol=di,byrow=T)
		x3=(matrix(c3,nrow=nn[3],ncol=di,byrow=T)
            + matrix(rnorm(nn[3]*di),ncol=di))
		c4=dd*rnorm(di)
		U4=matrix(c4,nrow=nn[4],ncol=di,byrow=T)
		x4=(matrix(c4,nrow=nn[4],ncol=di,byrow=T)
            + matrix(rnorm(nn[4]*di),ncol=di))

		x=rbind(x1,x2,x3,x4)
		Mu = rbind(U1,U2,U3,U4)

		ss=dist(rbind(x,c1,c2,c3,c4))
		d=matrix(0,nrow=nrow(x)+4,ncol=nrow(x)+4)
		d[row(d)>col(d)]=ss
		DD=d[(nrow(x)+1):(nrow(x)+4),1:nrow(x)]

		for(i in 1:ncol(DD)){ DD[cl[i],i]=DD[cl[i],i]+del}

		ff=apply(DD,2,which.min)
	    if(sum(ff==cl)==nrow(x)){
			condition <- FALSE
		}
	}

    ID <- sample(1:nrow(x),nrow(x),replace = FALSE)
    Data <- x[ID,]
    Mean <- Mu[ID,]

    list(x = Data, mean = Mean)
}


setting3 <- function()
{
    Data4center(di = 100, dd = 0.65)
}



Data10center <- function(di = 100, dd = 0.72){

	condition <- TRUE
	while(condition){
		del = 1
		nn=sample(c(50,100),size=10,replace=T)
		cl=c(rep(1,nn[1]),rep(2,nn[2]),rep(3,nn[3]),rep(4,nn[4]),rep(5,nn[5]),rep(6,nn[6]),rep(7,nn[7]),rep(8,nn[8]),rep(9,nn[9]),rep(10,nn[10]))

		c1=dd*rnorm(di)
		U1=matrix(c1,nrow=nn[1],ncol=di,byrow=T)
		x1=matrix(c1,nrow=nn[1],ncol=di,byrow=T) + matrix(rnorm(nn[1]*di),ncol=di)
		c2=dd*rnorm(di)
		U2=matrix(c2,nrow=nn[2],ncol=di,byrow=T)
		x2=matrix(c2,nrow=nn[2],ncol=di,byrow=T) + matrix(rnorm(nn[2]*di),ncol=di)
		c3=dd*rnorm(di)
		U3=matrix(c3,nrow=nn[3],ncol=di,byrow=T)
		x3=matrix(c3,nrow=nn[3],ncol=di,byrow=T) + matrix(rnorm(nn[3]*di),ncol=di)
		c4=dd*rnorm(di)
		U4=matrix(c4,nrow=nn[4],ncol=di,byrow=T)
		x4=matrix(c4,nrow=nn[4],ncol=di,byrow=T) + matrix(rnorm(nn[4]*di),ncol=di)
		c5=dd*rnorm(di)
		U5=matrix(c5,nrow=nn[5],ncol=di,byrow=T)
		x5=matrix(c5,nrow=nn[5],ncol=di,byrow=T) + matrix(rnorm(nn[5]*di),ncol=di)
		c6=dd*rnorm(di)
		U6=matrix(c6,nrow=nn[6],ncol=di,byrow=T)
		x6=matrix(c6,nrow=nn[6],ncol=di,byrow=T) + matrix(rnorm(nn[6]*di),ncol=di)
		c7=dd*rnorm(di)
		U7=matrix(c7,nrow=nn[7],ncol=di,byrow=T)
		x7=matrix(c7,nrow=nn[7],ncol=di,byrow=T) + matrix(rnorm(nn[7]*di),ncol=di)
		c8=dd*rnorm(di)
		U8=matrix(c8,nrow=nn[8],ncol=di,byrow=T)
		x8=matrix(c8,nrow=nn[8],ncol=di,byrow=T) + matrix(rnorm(nn[8]*di),ncol=di)
		c9=dd*rnorm(di)
		U9=matrix(c9,nrow=nn[9],ncol=di,byrow=T)
		x9=matrix(c9,nrow=nn[9],ncol=di,byrow=T) + matrix(rnorm(nn[9]*di),ncol=di)
		c10=dd*rnorm(di)
		U10=matrix(c10,nrow=nn[10],ncol=di,byrow=T)
		x10=matrix(c10,nrow=nn[10],ncol=di,byrow=T) + matrix(rnorm(nn[10]*di),ncol=di)


		x=rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
		Mu=rbind(U1,U2,U3,U4,U5,U6,U7,U8,U9,U10)

		ss=dist(rbind(x,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10))
		d=matrix(0,nrow=nrow(x)+10,ncol=nrow(x)+10)
		d[row(d)>col(d)]=ss
		DD=d[(nrow(x)+1):(nrow(x)+10),1:nrow(x)]

		for(i in 1:ncol(DD)){ DD[cl[i],i]=DD[cl[i],i]+del}

		ff=apply(DD,2,which.min)
	if(sum(ff==cl)==nrow(x)){
			condition <- FALSE
		}
	}

    ID <- sample(1:nrow(x),nrow(x),replace = FALSE)
    Data <- x[ID,]
    Mean <- Mu[ID,]

    list(x = Data, mean = Mean)
}


setting4 <- function()
{
    Data10center(di = 100, dd = 0.72)
}


Data4lognormal <- function(di = 16, dd = 1.2){
	condition <- TRUE
	while(condition){
		del = 1
		nn=sample(c(30,60),size=4,replace=T)
		cl=c(rep(1,nn[1]),rep(2,nn[2]),rep(3,nn[3]),rep(4,nn[4]))

		c1=dd*rnorm(di)
		u1=matrix(c1,nrow=nn[1],ncol=di,byrow=T)##
		x1=matrix(c1,nrow=nn[1],ncol=di,byrow=T) + matrix(rlnorm(nn[1]*di, 0, 0.5),ncol=di) - exp(0.25/2)
		c2=dd*rnorm(di)
		u2=matrix(c2,nrow=nn[2],ncol=di,byrow=T)##
		x2=matrix(c2,nrow=nn[2],ncol=di,byrow=T) + matrix(rlnorm(nn[2]*di, 0, 0.5),ncol=di) - exp(0.25/2)
		c3=dd*rnorm(di)
		u3=matrix(c3,nrow=nn[3],ncol=di,byrow=T)##
		x3=matrix(c3,nrow=nn[3],ncol=di,byrow=T) + matrix(rlnorm(nn[3]*di, 0, 0.5),ncol=di)- exp(0.25/2)
		c4=dd*rnorm(di)
		u4=matrix(c4,nrow=nn[4],ncol=di,byrow=T)##
		x4=matrix(c4,nrow=nn[4],ncol=di,byrow=T) + matrix(rlnorm(nn[4]*di, 0, 0.5),ncol=di)- exp(0.25/2)

		x=rbind(x1,x2,x3,x4)
		Mu=rbind(u1,u2,u3,u4)

		ss=dist(rbind(x,c1,c2,c3,c4))
		d=matrix(0,nrow=nrow(x)+4,ncol=nrow(x)+4)
		d[row(d)>col(d)]=ss
		DD=d[(nrow(x)+1):(nrow(x)+4),1:nrow(x)]

		for(i in 1:ncol(DD)){ DD[cl[i],i]=DD[cl[i],i]+del}

		ff=apply(DD,2,which.min)
	if(sum(ff==cl)==nrow(x)){
			condition <- FALSE
		}
	}

    ID <- sample(1:nrow(x),nrow(x),replace = FALSE)
    Data <- x[ID,]
    Mean <- Mu[ID,]
    list(x = Data, mean = Mean)
}


setting5 <- function()
{
    Data4lognormal(16,1.2)
}


Data3center <- function(number = 40)
{
     Centers <- mvrnorm(3, rep(0,20), 19*diag(20))
     u1 <- Centers[1,]
     u2 <- Centers[2,]
     u3 <- Centers[3,]

     condition <- TRUE
     while(condition){
	      obs1 <- (matrix(u1,nrow=number,ncol=20,byrow=T)
                   + matrix(rexp(number*20, rate = 1),ncol=20)-1)
		U1 <- matrix(u1,nrow=number,ncol=20,byrow=T)
		obs2 <- (matrix(u2,nrow=number,ncol=20,byrow=T)
                 + matrix(rexp(number*20, rate = 1/2),ncol=20)-2)
		U2 <- matrix(u2,nrow=number,ncol=20,byrow=T)
		obs3 <- (matrix(u3,nrow=number,ncol=20,byrow=T)
                 + matrix(rexp(number*20, rate = 1/5),ncol=20)-5)
		U3 <-  matrix(u3,nrow=number,ncol=20,byrow=T)

		if(sum(location(obs1,Centers)==1)==number
           & sum(location(obs2,Centers)==2)==number
           & sum(location(obs3,Centers)==3)==number){
			condition <- FALSE
		}
	}
	Data <- rbind(obs1,obs2,obs3)
	Mu <- rbind(U1,U2,U3)
	ID <- sample(1:nrow(Data),nrow(Data),replace = FALSE)
	Data <- Data[ID,]

	Mean <- Mu[ID,]
	list(x = Data, mean = Mean)
}


setting6 <- function()
{
    Data3center(40)
}


# seeds taken from random.org, uniform { 1, ..., 10000000 }
settings <- list("setting1" = list(simulate = setting1, seed = 4244250),
                 "setting2" = list(simulate = setting2, seed = 5513442),
                 "setting3" = list(simulate = setting3, seed = 5685887),
                 "setting4" = list(simulate = setting4, seed = 1243061),
                 "setting5" = list(simulate = setting5, seed = 7997590),
                 "setting6" = list(simulate = setting6, seed = 5427086))


