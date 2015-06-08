# code/cluster.R


# default seed chosen by random.org
cluster_kmeans <- function(x, centers, seed = 2651513, nstart = 100, ...)
{
    if (!is.na(seed)) {
        if ((exists0 <- exists(".Random.seed", globalenv()))) {
            seed0 <- .Random.seed
        }
        set.seed(seed)
    }

    cl <- stats::kmeans(x, centers, nstart=nstart, ...)

    if (!is.na(seed)) {
        if (exists0) {
            .Random.seed <- seed0
        } else {
            rm(".Random.seed", globalenv())
        }
    }

    cl
}


