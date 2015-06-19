#!/usr/bin/Rscript --vanilla

library("MASS")
library("parallel")
source("settings.R") # settings


# Use the L'Ecuyer RNG so that we can split the seeds, allowing
# each replicate to have a separate stream of random numbers.
rng_kind <- "L'Ecuyer-CMRG"
rng_normal_kind <- "Inversion"


nrep <- 100

for (s in names(settings)) {
    if (!dir.exists(s)) {
        dir.create(s)
    }
    filename <- file.path(s, "replicates.rds")
    if (!file.exists(filename)) {

        cat("generating replicates for '", s, "'\n", sep="")

        set.seed(settings[[s]]$seed, rng_kind, rng_normal_kind)
        replicates <- vector("list", nrep)

        pb <- txtProgressBar(0, nrep, style=3)
        for (r in seq_len(nrep)) {
            replicates[[r]] <- settings[[s]]$simulate()

            # split the RNG and save one stream with the replicate
            s0 <- .Random.seed
            replicates[[r]][["rng"]] <- list(seed = nextRNGSubStream(s0),
                                             kind = rng_kind,
                                             normal_kind = rng_normal_kind)
            .Random.seed <- nextRNGStream(s0)
            setTxtProgressBar(pb, r)
        }
        close(pb)

        saveRDS(replicates, filename)
    }
}
