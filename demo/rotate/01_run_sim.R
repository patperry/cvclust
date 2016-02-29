#!/usr/bin/Rscript --vanilla

library("RColorBrewer")


rot_sim <- function(corr, nrep=1000)
{
    # start with a covariance matrix with high correlation
    dim <- 5
    #cov <- matrix(c(1, corr, corr, 1), 2, 2)
    cov <- matrix(1, dim, dim)
    cov[1,2] <- corr
    cov[2,1] <- corr

    corr_rot <- rep(NA, nrep)

    for (r in seq_len(nrep)) {
        # generate a random rotation
        z <- matrix(rnorm(dim * dim), dim, dim)
        qr <- qr(z)
        q <- qr.Q(qr)
        sign <- sample(c(-1, 1), dim, replace=TRUE)
        rot <- q %*% diag(sign, dim)

        # compute the covariance matrix for the rotated data
        cov_rot <- t(rot) %*% cov %*% rot
        sd_rot <- sqrt(diag(cov_rot))

        # compute the correlation for the rotated data
        corr_rot[r] <- cov_rot[1,2] / (sd_rot[1] * sd_rot[2])
    }

    corr_rot
}


set.seed(0)

nrep <- 1000
corr <- seq(0, 1, len=100)
corr_rot_mean <- rep(NA, length(corr))
corr_rot_sd <- rep(NA, length(corr))

for (i in seq_along(corr)) {
    sim <- rot_sim(corr[i], nrep)
    corr_rot_mean[i] <- mean(abs(sim))
    corr_rot_sd[i] <- sd(abs(sim))
}

palette(brewer.pal(6, "Set1"))
plot(corr, corr_rot_mean, col=2)
segments(corr, corr_rot_mean - corr_rot_sd, corr, corr_rot_mean + corr_rot_sd,
         col=2)
abline(h=0.5, col=1)

i <- max(which(corr_rot_mean < 0.5))
abline(v=0.5 * (corr[i] + corr[i+1]), col=1, lty=2)

