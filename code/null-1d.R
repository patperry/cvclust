# code/null-1d.R
# experiments for the null case (1 cluster) in one dimesion


cluster <- function(x, centers, seed = 0, nstart = 100, ...)
{
    if (!is.na(seed)) {
        if ((exists0 <- exists(".Random.seed", globalenv()))) {
            seed0 <- .Random.seed
        }
        set.seed(seed)
    }

    cl <- kmeans(x, centers, nstart=nstart, ...)

    if (!is.na(seed)) {
        if (exists0) {
            .Random.seed <- seed0
        } else {
            rm(".Random.seed", globalenv())
        }
    }

    cl
}



set.seed(2651513) # from random.org

n <- 1000000

mu <- 1.0
mean <- c(-1, 1) * mu

eps <- rnorm(n)
g <- sample.int(2, n, TRUE)
x <- mean[g] + eps


