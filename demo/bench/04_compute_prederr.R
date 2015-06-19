#!/usr/bin/Rscript --vanilla

for (s in list.dirs(full.names=FALSE, recursive=FALSE)) {
    if (file.exists(file.path(s, "prederr.rds")))
        next

    cat("computing prediction error for '", s, "'\n", sep="")

    replicates <- readRDS(file.path(s, "replicates.rds"))
    kmeans <- readRDS(file.path(s, "kmeans.rds"))

    nrep <- length(replicates)
    prederr <- vector("list", nrep)

    for (r in seq_len(nrep)) {
        truth <- replicates[[r]]$mean
        kmax <- length(kmeans[[r]])
        prederr[[r]] <- rep(NA, kmax)
        for(k in seq_len(kmax)) {
            km <- kmeans[[r]][[k]]
            pred <- km$centers[km$cluster,]
            prederr[[r]][[k]] <- mean((truth - pred)^2)
        }
    }

    saveRDS(prederr, file.path(s, "prederr.rds"))
}
