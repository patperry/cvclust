#!/usr/bin/Rscript --vanilla

source("../../code/cluster.R") # cluster_kmeans
library("parallel")

kmax <- 15

options(cl.cores=max(1, detectCores() - 1))
cl <- makeCluster(getOption("cl.cores", 2))

clusterExport(cl=cl, varlist=c("cluster_kmeans", "kmax"))

for (s in list.dirs(full.names=FALSE, recursive=FALSE)) {
    f.replicates <- file.path(s, "replicates.rds")
    f.kmeans     <- file.path(s, "kmeans.rds")

    if (file.exists(f.replicates) && !file.exists(f.kmeans)) {
        cat("fitting k-means for '", s, "'\n", sep="")

        replicates <- readRDS(f.replicates)
        kmeans <- parLapply(cl, replicates, function(r) {
                km <- vector("list", kmax)
                for (k in seq_len(kmax)) {
                    km[[k]] <- cluster_kmeans(r$x, k)
                }
                km
            })

        saveRDS(kmeans, f.kmeans)
    }
}

stopCluster(cl)
