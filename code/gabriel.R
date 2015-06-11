# code/gabriel.R
#
# Depends:
#   library("bcv")
#   library("MASS")
#   source("classify.R")
#   source("cluster.R")
#


cv.kmeans.gabriel <- function(x, krow = 5, kcol = 2, maxcenters = 10,
                              classify.method = c("lda-equal", "lda-proportions", "svm"))
{
    classify.method <- match.arg(classify.method)
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    if (n < 2) 
        stop("x should have at least two rows")
    if (p < 2) 
        stop("x should have at least two columns")
    if ((krow > n) || (krow <= 1)) 
        stop("krow outside allowable range")
    if ((kcol > p) || (kcol <= 1)) 
        stop("kcol outside allowable range")
    if (maxcenters <= 0) 
        stop("maxcenters should be positive")

    krow.o <- krow
    krow <- bcv:::round.fold(n, krow)
    kcol.o <- kcol
    kcol <- bcv:::round.fold(p, kcol)
    if (krow != krow.o) 
        warning("krow has been set to ", krow)
    if (kcol != kcol.o) 
        warning("kcol has been set to ", kcol)

    s.r <- bcv:::choose.sets(n, krow)
    s.c <- bcv:::choose.sets(p, kcol)
    n0 <- n - max(table(s.r))
    p0 <- p - max(table(s.c))
    maxcenters.o <- maxcenters
    maxcenters <- min(n0, round(maxcenters.o))
    if (!missing(maxcenters) && maxcenters != maxcenters.o) 
        warning("maxcenters has been set to ", maxcenters)

    if (classify.method == "lda-equal") {
        classify <- function(x, grouping)
            classify_lda(x, grouping, prior="equal")
    } else if (classify.method == "lda-proportions") {
        classify <- function(x, grouping)
            classify_lda(x, grouping, prior="proportions")
    } else if (classify.method == "svm") {
        stop("not yet implemented")
    }

    msep <- matrix(NA, krow * kcol, maxcenters)

    for (k in seq_len(krow)) {
        for (l in seq_len(kcol)) {
            test <- s.r == k
            train <- !test
            response <- s.c == l
            predictor <- !response

            for (centers in seq_len(maxcenters)) {
                if (centers == 1) {
                    fit <- colMeans(x[train,response,drop=FALSE])
                    err <- scale(x[test,response,drop=FALSE], center=fit, scale=FALSE)
                } else {
                    cl <- cluster_kmeans(x[train,response,drop=FALSE], centers=centers)
                    cluster <- factor(cl$cluster, levels=seq_len(centers))
                    fit <- classify(x[train,predictor,drop=FALSE], cluster)
                    pred <- predict(fit, x[test,predictor,drop=FALSE])$class
                    err <- x[test,response,drop=FALSE] - cl$centers[pred,,drop=FALSE]
                }

                msep[k + (l - 1) * krow, centers] <- mean(err^2)
            }
        }
    }

    msep.mean <- colMeans(msep)
    centers <- which.min(msep.mean)
    list(msep, centers=centers)
}

