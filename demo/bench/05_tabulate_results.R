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
    "setting3" = 4L,
    "setting4" = 10L,
    "setting5" = 4L,
    "setting6" = 3L)

method.names <- list(
    "oracle" = "Oracle",
    "gap" = "Gap",
    "BIC" = "Gaussian-Mix",
    "CH" = "CH",
    "Hartigan" = "Hartigan",
    "Jump" = "Jump",
    "PS" = "Pred.~Strength",
    "Stab" = "Stability",
#    "gabriel-nearest-2x2" = "Gabriel CV ($2 \\times 2$)",
    "gabriel-nearest-5x2" = "Gabriel CV",
#    "gabriel-lda-equal-2x2" = "Gabriel CV (LDA Equal; $2 \\times 2$)",
#    "gabriel-lda-equal-5x2" = "Gabriel CV (LDA Equal; $5 \\times 2$)",
#    "gabriel-lda-proportion-2x2" = "Gabriel (CV LDA Prop.; $2 \\times 2$)",
#    "gabriel-lda-proportion-5x2" = "Gabriel (CV LDA Prop.; $5 \\times 2$)",
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

print_table <- function(setting, summary, pe=FALSE, zero=FALSE)
{
    cat("\\textit{", setting.names[[setting]], "} & \\\\*\n", sep="")

    kmax <- length(summary[["oracle"]]$counts)

    for (m in names(method.names)) {
        s <- summary[[m]]
        if (is.null(s) || (m == "oracle" && !pe))
            next
        cat(method.names[[m]])
        if (exclude(setting, m))
            cat("\\textsuperscript{$\\ast$}")
        for (k in seq_len(kmax)) {
            cat(" & ")
            if (exclude(setting, m)) {
                cat("$\\cdot$")
            } else if (k == nclust[[setting]]) {
                cat("\\textbf{", s$counts[k], "}", sep="")
            } else {
                if (zero || s$counts[k] != 0) {
                    cat(s$counts[k])
                } else {
                    cat("$\\cdot$")
                }
            }
        }
        if (exclude(setting, m)) {
            cat("& $\\cdot$")
        } else {
            cat(" & ")
            if (zero || s$count_na != 0) {
                cat(s$count_na)
            } else {
                cat("$\\cdot$")
            }
        }

        if (pe) {
            if (m == "oracle") {
                cat(" & 1")
            } else if (exclude(setting, m)) {
                cat(" & $\\cdot$")
            } else {
                cat(sprintf(" & %.1f $\\pm$ %.1f",
                            s$prederr_rel$mean,
                            s$prederr_rel$sd))
            }
        }
        cat(" \\\\*\n")
    }
}

for (s in list.dirs(full.names=FALSE, recursive=FALSE)) {
    cat("tabulating results for '", s, "'\n", sep="")
    prederr <- readRDS(file.path(s, "prederr.rds"))
    nrep <- length(prederr)
    kmax <- length(prederr[[1]])

    nclusters <- list()
    for (filename in list.files(file.path(s, "method"), "[.]rds$")) {
        m <- substr(filename, 1, nchar(filename) - 4)
        nclusters[[m]] <- readRDS(file.path(s, "method", filename))
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
            if (is.na(nc[r])) {
                x[r] <- NA
            } else {
                x[r] <- prederr[[r]][nc[r]]
            }
        }
        n <- sum(!is.na(x))
        list(mean = mean(x, na.rm=TRUE),
             sd = sd(x, na.rm=TRUE),
             se_mean = sd(x, na.rm=TRUE) / sqrt(n))
    }

    summary <- list()
    for (m in names(nclusters)) {
        count_na <- sum(is.na(nclusters[[m]]))
        counts <- tabulate(nclusters[[m]], kmax)
        summary[[m]] <- list(
            count_na = count_na,
            counts = counts,
            proportions = counts / (length(nclusters[[m]]) - count_na),
            prederr = summarize(prederr, nclusters[[m]]),
            prederr_rel = summarize(prederr_rel, nclusters[[m]]))
    }

    sink(file.path(s, "results.tex"))
    print_table(s, summary)
    sink()
}
    

