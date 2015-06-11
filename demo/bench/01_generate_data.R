#!/usr/bin/Rscript --vanilla

library("MASS")
source("settings.R") # settings

nrep <- 100

for (s in names(settings)) {
    if (!dir.exists(s)) {
        dir.create(s)
    }
    filename <- file.path(s, "replicates.rds")
    if (!file.exists(filename)) {

        cat("generating replicates for '", s, "'\n", sep="")

        set.seed(settings[[s]]$seed)
        replicates <- vector("list", nrep)

        pb <- txtProgressBar(0, nrep, style=3)
        for (r in seq_len(nrep)) {
            replicates[[r]] <- settings[[s]]$simulate()
            setTxtProgressBar(pb, r)
        }
        close(pb)

        saveRDS(replicates, filename)
    }
}
