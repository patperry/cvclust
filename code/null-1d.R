# code/null-1d.R
# experiments for the null case (1 cluster) in one dimesion

library("MASS") # lda
library("RColorBrewer") # brewer.pal

palette(brewer.pal(6, "Set1"))


# default seed chosen by random.org
cluster <- function(x, centers, seed = 2651513, nstart = 100, ...)
{
    if (!is.na(seed)) {
        if ((exists0 <- exists(".Random.seed", globalenv()))) {
            seed0 <- .Random.seed
        }
        set.seed(seed)
    }

    cl <- kmeans(x, centers, nstart=nstart, ...)

    if (!is.na(seed)) {
        if (exists0) {
            .Random.seed <- seed0
        } else {
            rm(".Random.seed", globalenv())
        }
    }

    cl
}





set.seed(0)

ntrain <- 100000
ntest <- ntrain
n <- ntest + ntrain
train <- seq_len(ntrain)
test <- ntrain + seq_len(ntest)

predictor <- 1
response <- 2

z <- matrix(rnorm(n * 2), n, 2)

cverr <- function(correlation, centers)
{
    r <- correlation
    coef <- cbind(rbind(sqrt((1 + r) / 2),  sqrt((1 - r) / 2)),
                  rbind(sqrt((1 + r) / 2), -sqrt((1 - r) / 2)))
    x <- z %*% coef

    if (centers == 1) {
        fit <- colMeans(x[train,response,drop=FALSE])
        err <- scale(x[test,response], center=fit, scale=FALSE)
    } else {
        cl <- cluster(x[train,response], centers=centers)
        cluster <- factor(cl$cluster, levels=seq_len(centers))
        fit <- lda(x[train,predictor,drop=FALSE], cluster)
        pred <- predict(fit, x[test,predictor,drop=FALSE])$class
        err <- x[test,response] - cl$centers[pred,]
    }

    mse <- mean(err^2)
    mse
}


correlation <- seq(-1, 1, length.out=21)
mse1 <- sapply(correlation, function(r) cverr(r, 1))
mse2 <- sapply(correlation, function(r) cverr(r, 2))
mse3 <- sapply(correlation, function(r) cverr(r, 3))
mse4 <- sapply(correlation, function(r) cverr(r, 4))
mse8 <- sapply(correlation, function(r) cverr(r, 8))
mse15 <- sapply(correlation, function(r) cverr(r, 15))

data <- data.frame(correlation = rep(correlation, 6),
                   mse = c(mse1, mse2, mse3, mse4, mse8, mse15),
                   centers = factor(rep(c(1, 2, 3, 4, 8, 15),
                                    each=length(correlation))))



mar <- c(3.25,3.25,0.75, 4.50 + 0.75)
mai <- mar * par("csi")
w <- h <- 3

pdf("fig.pdf", width=sum(w, mai[c(2,4)]), height=sum(h, mai[c(1,3)]))

par(las=1, mai=mai, mgp=c(2.25,0.75,0), ps=11, tcl=-0.4)
plot(data$correlation, data$mse, type="n",
     xlab="Correlation", ylab="CV Error")
axis(3, label=FALSE)
axis(4, label=FALSE)

for (i in seq_len(nlevels(data$centers))) {
    k <- levels(data$centers)[i]
    dk <- subset(data, centers == k)
    points(dk$correlation, dk$mse, pch=i, col=i)
}


centers <- levels(data$centers)
ncenters <- nlevels(data$centers)

usr <- par("usr")
cxy <- par("cxy")
legend.x <- usr[2] + 3 * cxy[1]
legend.y <- usr[3]

legend(legend.x, legend.y, xjust=0, yjust=0, xpd=TRUE, bty="n",
       title="Clusters", title.adj=0,
       legend=centers, pch=1:ncenters, col=1:ncenters)

dev.off()
