#!/usr/bin/Rscript --vanilla


for (s in list.dirs(full.names=FALSE, recursive=FALSE)) {
    replicates <- readRDS(file.path(s, "replicates.rds"))
    kmeans <- readRDS(file.path(s, "kmeans.rds"))

    nclusters <- list()
    for (filename in list.files(file.path(s, "method"), "[.]rds$")) {
        m <- substr(filename, 1, nchar(filename) - 4)
        nclusters[[m]] <- readRDS(file.path(s, "method", filename))
    }
}
    

