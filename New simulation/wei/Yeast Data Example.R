
Data <- read.table("Yeast/cdc15.txt",header=T,sep="\t")
head(Data)
DATA = Data[,52:68]

##impute NAs by the column mean
Test <- as.matrix(DATA)
for(i in 1:ncol(Test)){
 	IDs <- which(is.na(Test[,i]));
	Impute <- median(Test[,i],na.rm=T);
	Test[,i][IDs] <- Impute;
}
##impute NAs by K-NN with k =5
##library("DMwR")
##Test <- as.matrix(DATA)
##Test <- knnImputation(Test,k=15)

##select the top 3000/2945 most variable genes(s.d/mean)
Rank <- rep(NA,nrow(Test))

for(i in 1:nrow(Test)){
	Rank[i] = sd(Test[i,])/mean(Test[i,])
}

Order <- order(Rank,decreasing = T)
Id <- Order[1:2945]
Test <- Test[Id,]
Gene <- as.character(Data$X)[Id]
Gene <- as.character(lapply(Gene,function(x)substr(x,1,7)))

##variance normalize each gene
Test <- Test[,-c(10,11)]

for(i in 1:nrow(Test)){
	Mean = mean(Test[i,]);
	SD = sd(Test[i,]);
	for(j in 1:ncol(Test)){
		Test[i,j] = (Test[i,j]-Mean)/SD
	}
}
################################################################################

set.seed(1)
K.original <- cv.kmeans.gabriel(Test, 5, 2, maxcenters=40, classify.method="nearest")$centers
K.original
DATA_new <- Uncorrelate2(Test,K.original)

K2<- cv.kmeans.gabriel(DATA_new, 5, 2, maxcenters=40, classify.method="nearest")$centers
K2

####gabriel_cor_correct(Test, maxcenters=40, type = 2)
#set.seed(1)		
#Gap <- clusGap(Test, FUN = kmeans, K.max =45)
#which(Gap[[1]][,3] == max(Gap[[1]][,3]))
			
#set.seed(1)	
#mcluster <- Mclust(Test, G = 20:40)
#mcluster$G
###################################################################################						
###==============================================================
Data <- as.data.frame(Test)
Data$Gene = Gene

##set.seed(1)
##Kfit <- kmeans(Data[,1:15],9, nstart = 100)
set.seed(12)
Kfit <- kmeans(DATA_new,5,nstart = 100)
Data$Cluster <- Kfit$cluster

Names_cluster <- c("Cluster 1; size=514","Cluster 2; size=583","Cluster 3; size=682","Cluster 4; size=563","Cluster 5; size=603")
##Names_cluster <- c("Cluster 1; size=467","Cluster 2; size=266","Cluster 3; size=408","Cluster 4; size=358","Cluster 5; size=216","Cluster 6; size=317","Cluster 7; size=315","Cluster 8; size=352","Cluster 9; size=246")

for(i in 1:5){
 	Subset <- Data[Data$Cluster==i,1:15];
	print(nrow(Subset));}
	Means = colMeans(Subset);
	Sds = apply(Subset, 2, sd);
	Summary <- cbind(rep(Names_cluster[i],15),1:15,unname(Means),unname(Sds))
	if(i==1){
	  Sum_data = Summary
	}else{
	  Sum_data = rbind(Sum_data,Summary) 
	}
	#plot(1:15,Means,type="l");
}
Sum_data<- as.data.frame(Sum_data)
names(Sum_data) <- c("Cluster","X","Y","Sd")
Sum_data$Y<- as.numeric(as.character(Sum_data$Y))
Sum_data$X<- as.numeric(as.character(Sum_data$X))
Sum_data$Sd<- as.numeric(as.character(Sum_data$Sd))

Sum_data$Upper = Sum_data$Y+Sum_data$Sd
Sum_data$Lower = Sum_data$Y-Sum_data$Sd

library(ggplot2)
pdf("9_clusters.pdf",width=9)
ggplot(Sum_data, aes(X, Y, group = Cluster))+labs(title = "", x = "", y = "")+geom_line(size = 1.3)+facet_wrap(~Cluster,nrow=3)+geom_errorbar(aes(ymax = Upper, ymin=Lower),colour = "black")
dev.off()
##-----------------------------------------------------------
Types <- read.table("go_slim_mapping.txt",header=F,sep="\t")
Types <- Types[Types[,4]=="P",c(1,5)] ## biological process
names(Types)<- c("Gene","Process")
Types$Gene <- as.character(Types$Gene)
Types$Gene <- as.character(lapply(Types$Gene,function(x)substr(x,1,7)))
Types$Process <- as.character(Types$Process)

Final <- merge(Data,Types,by="Gene",all.x=TRUE)
write.csv(Final, "Final_Yeast.csv")
##-----------------------------
Final2 <- read.csv("Final_Yeast.csv")
Final2$Gene <-as.character(Final2$Gene)
Final2$Process <-as.character(Final2$Process )
 
reference_table <- ddply(Final2,.(Process),summarise, Total = length(Gene))

SIZE <- c(514,583,682,563,603)

for(k in 1:5){
 	subset = Final2[Final2$Cluster==k,]
	print(paste("cluster",k))
	Process.table = ddply(subset,.(Process),summarise, Total = length(Gene))
	Process.table$Grand = NA;
	Process.table$P = NA;
	for(j in 1:nrow(Process.table)){
		z <- Process.table[j,]$Total
		m <-  reference_table[reference_table[,1]==Process.table[j,]$Process,2][1]
		Process.table[j,]$Grand = m
		Process.table[j,]$P = 1-phyper(z-1,m,2945-m,SIZE[k])
	}
	print(Process.table[Process.table$P <= 10^(-4),])
}







