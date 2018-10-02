#!/usr/bin/Rscript --vanilla


library("MASS")
source("../../code/classify.R")
source("../../code/cluster.R")


set.seed(7627579) # from random.org

ntrain <- 10000
ntest <- ntrain
n <- ntest + ntrain
train <- seq_len(ntrain)
test <- ntrain + seq_len(ntest)

predictor <- 1
response <- 2

nrep <- 10

sim <- NULL

for (r in seq_len(nrep)) {
    z <- matrix(rnorm(n * 2), n, 2)

    cverr <- function(correlation, centers, prior)
    {
        r <- correlation
        coef <- cbind(rbind(sqrt((1 + r) / 2),  sqrt((1 - r) / 2)),
                      rbind(sqrt((1 + r) / 2), -sqrt((1 - r) / 2)))
        x <- z %*% coef

        if (centers == 1) {
            fit <- colMeans(x[train,response,drop=FALSE])
            err <- scale(x[test,response], center=fit, scale=FALSE)
        } else {
            cl <- cluster_kmeans(x[train,response], centers=centers)
            cluster <- factor(cl$cluster, levels=seq_len(centers))
            fit <- classify_lda(x[train,predictor,drop=FALSE], cluster, prior)
            pred <- predict(fit, x[test,predictor,drop=FALSE])$class
            err <- x[test,response] - cl$centers[pred,]
        }

        mse <- mean(err^2)
        mse
    }


    correlation <- seq(-1, 1, length.out=21)
    for (prior in c("equal", "proportions")) {
        for (centers in c(1, 2, 3, 4, 5)) {
            mse <- sapply(correlation, function(r) cverr(r, centers, prior))

            sim <- rbind(sim,
                         data.frame(replicate = r,
                                    prior = prior,
                                    correlation = correlation,
                                    mse = mse,
                                    centers = centers))
        }
    }
}

sim$replicate <- as.factor(sim$replicate)
sim$centers <- as.factor(sim$centers)

write.csv(sim, "nullcorr.csv", row.names=FALSE)

