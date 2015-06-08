# code/classify.R
#
# Depends:
#   library("MASS")


classify_lda <- function(x, grouping, prior=c("proportions", "equal"))
{
    g <- as.factor(grouping)
    prior <- match.arg(prior)

    n <- length(grouping)
    ng <- nlevels(g)

    if (prior == "equal") {
        prior <- rep(1/ng, ng)
    } else {
        counts <- as.vector(table(g))
        prior <- counts / n
    }

    MASS::lda(x, g, prior)
}

