# figure/elbow

library("MASS")         # mvrnorm
library("RColorBrewer") # brewer.pal

source("../code/cluster.R") # cluster_kmeans


palette(brewer.pal(6, "Set1"))

##---------------------Top plot of figure 1-----------------------------------

set.seed(3) # seed chosen to make well-separated clusters

x1 <- mvrnorm(20, c(1.7, 3.0), 0.15 * diag(2))
x2 <- mvrnorm(20, c(1.0, 1.0), 0.15 * diag(2))
x3 <- mvrnorm(20, c(3.0, 1.5), 0.15 * diag(2))
x4 <- mvrnorm(20, c(3.7, 3.7), 0.10 * diag(2))

data <- rbind(data.frame(x=x1[,1], y=x1[,2], group="1"),
              data.frame(x=x2[,1], y=x2[,2], group="2"),
              data.frame(x=x3[,1], y=x3[,2], group="3"),
              data.frame(x=x4[,1], y=x4[,2], group="4"))



mar <- c(3.25, 3.25, 1.75, 1.75)
mai <- mar * par("csi")
w <- h <- 3
mypar <- list(las=1, mai=mai, mgp=c(2.25, 0.75, 0), ps=11, tcl=-0.4)

width <- w + sum(mai[c(1,3)])
height <- h + sum(mai[c(2,4)])



pdf("elbow-correct-data.pdf", width=width, height=height)
par(mypar)

plot(data$x, data$y, type="n", xlab="X", ylab="Y")
points(data$x, data$y, pch=as.integer(data$group),
       col=as.integer(data$group))
axis(3, labels=FALSE)
axis(4, labels=FALSE)
dev.off()


centers <- 1:10
tot.withinss <- rep(NA, length(centers))
for (i in seq_along(centers)) {
 	cl <- cluster_kmeans(cbind(data$x, data$y), centers[i])
    tot.withinss[i] <- cl$tot.withinss
}


pdf("elbow-correct-withinss.pdf", width=width, height=height)
par(mypar)

plot(centers, tot.withinss, type="b",
     xlab = "Clusters",
     ylab = "Within SS")
axis(3, labels=FALSE)
axis(4, labels=FALSE)

dev.off()


##---------------------Bottom plot of figure 1---------------------------------

set.seed(1) # seed chosen to make well-separated clusters

x1 <- 0.3 * mvrnorm(100, c( 5,  5), 3.0 * diag(2))
x2 <- 0.3 * mvrnorm( 20, c( 5, 12),       diag(2))
x3 <- 0.3 * mvrnorm( 10, c( 9,  9), 0.5 * diag(2))
x4 <- 0.3 * mvrnorm(100, c(12, 12),       diag(2))

data <- rbind(data.frame(x=x1[,1], y=x1[,2], group="1"),
              data.frame(x=x2[,1], y=x2[,2], group="2"),
              data.frame(x=x3[,1], y=x3[,2], group="3"),
              data.frame(x=x4[,1], y=x4[,2], group="4"))

pdf("elbow-incorrect-data.pdf", width=width, height=height)
par(mypar)

plot(data$x, data$y, type="n", xlab="X", ylab="Y")
points(data$x, data$y, pch=as.integer(data$group),
       col=as.integer(data$group))
axis(3, labels=FALSE)
axis(4, labels=FALSE)

dev.off()


centers <- 1:10
tot.withinss <- rep(NA, length(centers))
for (i in seq_along(centers)) {
 	cl <- cluster_kmeans(cbind(data$x, data$y), centers[i])
    tot.withinss[i] <- cl$tot.withinss
}


pdf("elbow-incorrect-withinss.pdf", width=width, height=height)
par(mypar)

plot(centers, tot.withinss, type="b",
     xlab = "Clusters", ylab = "Within SS")
axis(3, labels=FALSE)
axis(4, labels=FALSE)

dev.off()
