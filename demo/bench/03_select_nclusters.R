#!/usr/bin/Rscript --vanilla

library("bcv")
library("cluster")
library("devtools")
library("e1071")
library("mclust")
library("MASS")
library("parallel")
load_all("../../lib/fpc", export_all=FALSE)
load_all("../../lib/NbClust", export_all=FALSE)
source("../../code/classify.R")
source("../../code/cluster.R")
source("../../code/gabriel.R")
source("../../code/jump.R")
source("../../code/wold.R")
source("methods.R")


options(cl.cores=max(1, detectCores() - 1))
cl <- makeCluster(getOption("cl.cores", 2))

kmax <- 15

for (s in list.dirs(full.names=FALSE, recursive=FALSE)) {
    f.replicates <- file.path(s, "replicates.rds")
    if (!file.exists(f.replicates))
        next

    replicates <- readRDS(f.replicates)
    nrep <- length(replicates)

    if (!dir.exists(file.path(s, "method")))
        dir.create(file.path(s, "method"))

    for (m in names(methods)) {
        f.method <- file.path(s, "method", paste0(m, ".rds"))

        if (!file.exists(f.method)) {
            cat("applying method '", m, "' to '", s, "'\n", sep="")

            set.seed(5453546) # from random.org
            method <- methods[[m]]
            nclusters <- integer(nrep)

            clusterExport(cl=cl, varlist=c("replicates", "method", "kmax",
                                           "classify_lda",
                                           "cluster_kmeans",
                                           "compute.jump",
                                           "cv.kmeans.gabriel",
                                           "Impute",
                                           "jump",
                                           "kmeans.rndstart",
                                           "mclustBIC",
                                           "WoldIter",
                                           "Wold_holdout"))
            nclusters <- parSapply(cl, seq_len(nrep), function(r) {
                    x <- replicates[[r]]$x
                    tryCatch(method(x, kmax),
                             error=function(e) NA_integer_)
                })

            nclusters <- as.integer(nclusters)
            saveRDS(nclusters, f.method)
        }
    }
}

stopCluster(cl)

