#!/usr/bin/Rscript --vanilla

library("bcv")
library("cluster")
library("devtools")
library("e1071")
library("mclust")
library("MASS")
load_all("../../lib/fpc")
load_all("../../lib/NbClust")
source("../../code/classify.R")
source("../../code/cluster.R")
source("../../code/gabriel.R")
source("../../code/jump.R")
source("../../code/wold.R")
source("methods.R")


kmax <- 10

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
            nclusters <- integer(nrep)

            pb <- txtProgressBar(0, nrep, style=3)
            for (r in seq_along(replicates)) {
                x <- replicates[[r]]$x
                nclusters[r] <- methods[[m]](x, kmax)
                setTxtProgressBar(pb, r)
            }
            close(pb)

            nclusters <- as.integer(nclusters)
            saveRDS(nclusters, f.method)
        }
    }
}
