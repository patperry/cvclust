#!/usr/bin/Rscript --vanilla

setting.names <- list(
    "setting1" = "Setting 1",
    "setting2" = "Setting 2",
    "setting3" = "Setting 3",
    "setting4" = "Setting 4",
    "setting5" = "Setting 5",
    "setting6" = "Setting 6")

nclust <- list(
    "setting1" = 1L,
    "setting2" = 2L,
    "setting3" = 3L,
    "setting4" = 4L,
    "setting5" = 10L,
    "setting6" = 4L)

method.names <- list(
    "oracle" = "Oracle",
    "gap" = "Gap",
    "BIC" = "Gaussian-Mix",
    "CH" = "CH",
    "Hartigan" = "Hartigan",
    "Jump" = "Jump",
    "PS" = "Prediction Strength",
    "Stab" = "Stability",
    "gabriel-lda-equal" = "Gabriel CV",
    "wold" = "Wold CV")

exclude <- function(setting, method)
{
    if (method %in% c("CH", "Hartigan", "Stab")) {
        if (setting %in% c("setting1")) {
            return(TRUE)
        }
    }
    FALSE
}

print_table <- function(setting, summary)
{
    cat("\\textit{", setting.names[[setting]], "} & \\\\\n", sep="")

    kmax <- length(summary[["oracle"]]$counts)

    for (m in names(method.names)) {
        s <- summary[[m]]
        if (is.null(s))
            next
        cat(method.names[[m]])
        for (k in seq_len(kmax)) {
            cat(" & ")
            if (exclude(setting, m)) {
                cat("\\textendash")
            } else if (k == nclust[[setting]]) {
                cat("\\textbf{", s$counts[k], "}", sep="")
            } else {
                cat(s$counts[k])
            }
        }
        if (m == "oracle") {
            cat(" & 1")
        } else if (exclude(setting, m)) {
            cat(" & \\textendash")
        } else {
            cat(sprintf(" & %.1f $\\pm$ %.1f",
                        s$prederr_rel$mean,
                        s$prederr_rel$sd))
        }
        cat(" \\\\\n")
    }
}

for (s in list.dirs(full.names=FALSE, recursive=FALSE)) {
    replicates <- readRDS(file.path(s, "replicates.rds"))
    kmeans <- readRDS(file.path(s, "kmeans.rds"))

    nclusters <- list()
    for (filename in list.files(file.path(s, "method"), "[.]rds$")) {
        m <- substr(filename, 1, nchar(filename) - 4)
        nclusters[[m]] <- readRDS(file.path(s, "method", filename))
    }

    # compute prediction error
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

    # compute oracle, relative prediction error
    oracle <- integer(nrep)
    prederr_rel <- vector("list", nrep)
    for (r in seq_len(nrep)) {
        oracle[r] <- which.min(prederr[[r]])
        prederr_rel[[r]] <- prederr[[r]] / prederr[[r]][oracle[r]]
    }
    nclusters[["oracle"]] <- oracle

    # summarize methods
    summarize <- function(prederr, nc)
    {
        x <- numeric(nrep)
        for (r in seq_len(nrep)) {
            x[r] <- prederr[[r]][nc[r]]
        }
        list(mean = mean(x), sd = sd(x), se_mean = sd(x) / sqrt(nrep))
    }

    summary <- list()
    for (m in names(nclusters)) {
        summary[[m]] <- list(
            counts = tabulate(nclusters[[m]], kmax),
            prederr = summarize(prederr, nclusters[[m]]),
            prederr_rel = summarize(prederr_rel, nclusters[[m]]))
    }

    sink(file.path(s, "results.tex"))
    print_table(s, summary)
    sink()
}
    

