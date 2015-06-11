# demo/bench/methods.R
#
# Depends:
#   library("bcv")
#   library("devtools")
#   library("cluster")
#   library("e1071")
#   library("mclust")
#   library("MASS")
#   load_all("../../lib/fpc")
#   load_all("../../lib/NbClust")
#   source("../../lib/NbClust.R")
#   source("../../code/classify.R")
#   source("../../code/cluster.R")
#   source("../../code/gabriel.R")
#   source("../../code/jump.R")
#   source("../../code/wold.R")
#   

methods <- list(
    "gabriel-lda-equal" = function(x, maxcenters) {
        cv <- cv.kmeans.gabriel(x, 2, 2, maxcenters,
                                classify.method="lda-equal")
        cv$centers
    },
    "wold" = function(x, maxcenters) {
        Wold_holdout(x, 5, max.k=maxcenters, Errortol=0.01)
    },
    "gap" = function(x, maxcenters) {
        Gap <- cluster::clusGap(x, FUN = cluster_kmeans, K.max = maxcenters)
        which.max(Gap[[1]][,3])
    },
    "BIC" = function(x, maxcenters) {
        mcluster <- mclust::Mclust(x, G = 1:maxcenters)
        mcluster$G
    },
    "CH" = function(x, maxcenters) {
        Ch <- NbClust::NbClust(x, min.nc = 2, max.nc = maxcenters,
                               method = "kmeans", index = "ch")
        Ch$Best.nc[[1]]
    },
    "Hartigan" = function(x, maxcenters) {
        Hartigan <- NbClust::NbClust(x, min.nc = 2, max.nc = maxcenters,
                                     method = "kmeans", index = "hartigan")
        Hartigan$Best.nc[[1]]
    },
    "Jump" = function(x, maxcenters) {
        Jump <- jump(x, plotjumps=FALSE, trace=FALSE)
        Jump$maxjump
    },
    "PS" = function(x, maxcenters) {
        PS <- fpc::prediction.strength(x, Gmin=2, Gmax=maxcenters,
                                       clustermethod=kmeansCBI)
        PS$optimalk
    },
    "Stab" = function(x, maxcenters) {
        SB <- fpc::nselectboot(x, clustermethod=kmeansCBI,
                               classification="centroid",
                               krange=2:maxcenters)
        SB$kopt
    })
