#!/usr/bin/Rscript --vanilla

source("../../code/cluster.R") # cluster_kmeans

kmax <- 15


for (s in list.dirs(full.names=FALSE, recursive=FALSE)) {
    f.replicates <- file.path(s, "replicates.rds")
    f.kmeans     <- file.path(s, "kmeans.rds")

    if (file.exists(f.replicates) && !file.exists(f.kmeans)) {
        cat("fitting k-means for '", s, "'\n", sep="")

        replicates <- readRDS(f.replicates)
        nrep <- length(replicates)
        kmeans <- vector("list", nrep)

        pb <- txtProgressBar(0, nrep, style=3)
        for (r in seq_len(nrep)) {
            kmeans[[r]] <- vector("list", kmax)
            for (k in seq_len(kmax)) {
                kmeans[[r]][[k]] <- cluster_kmeans(replicates[[r]]$x, k)
            }
            setTxtProgressBar(pb, r)
        }
        close(pb)

        saveRDS(kmeans, f.kmeans)
    }
}

