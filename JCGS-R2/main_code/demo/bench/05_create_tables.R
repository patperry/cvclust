setwd("C:/Users/fuwei/Desktop/JCGS-R2/main_code/demo/bench") ## set the working directory correctly

library('xtable')

nclusters <- list("setting1" = 6L,
		     "setting2" = 3L,
			 "setting3" = 8L,
			 "setting4" = 3L,
			 "setting5" = 5L)

Scale_X <-  list("setting1" = seq(0.0,0.9,0.1),
		     "setting2" = 6*seq(0,9,1),
			 "setting3" = seq(10,100,10),
			 "setting4" = c(1,seq(5,45,5)),
			 "setting5" = seq(2,11,1))

Label_X <- list("setting1" = "Rho",
		    "setting2" = "Noise Dimension",
			"setting3" = "Data Dimension",
			"setting4" = "Max Variance Ratio",
			"setting5" = "Degree of freedom")



for (s in list.dirs(full.names=FALSE, recursive=FALSE)){
	print(s)
	print('------------------------------------------------')
	rm(Result.path)

	####----------------------Read from RDS and count the K-mean results-----------------
	Counts <- function(x){
		vector <- rep(NA,11)
		for(j in 1:10){
			vector[j] <- sum(x==j,na.rm=TRUE)
		}
		vector[11] <- 100-sum(vector[1:10]) ## vector[11] stores the NAs
		return(vector)
	}
	###----------------------------------------------------------------------
	Result.path <- file.path(s,"result.rds")
	if(!file.exists(Result.path)){stop("file path doesn't exists")}

	Fit_result <- readRDS(Result.path)

	## create Latex table to be exported
	for(i in 1:length(Fit_result)){
		count <- as.data.frame(apply(Fit_result[[i]], 2, Counts))
		colnames(count) <- as.character(Scale_X[[s]])
		rownames(count) <- c("1","2","3","4","5","6","7","8","9","10","NA")		
		print(xtable(count,digits=0,caption = names(Fit_result)[i]),table.placement="H")
	}
	print('------------------------------------------------')
}