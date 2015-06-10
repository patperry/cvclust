#!/usr/bin/Rscript --vanilla


library("RColorBrewer") # brewer.pal

data <- read.csv("nullcorr.csv")
data$replicate <- as.factor(data$replicate)
data$centers <- as.factor(data$centers)

mar <- c(3.25,3.25,0.75, 4.50 + 0.75)
csi <- 0.2 # default value of par("csi")
mai <- mar * csi
w <- h <- 3

for (prior in levels(data$prior)) {
    filename <- paste0(prior, ".pdf")
    pdf(filename, width=sum(w, mai[c(2,4)]), height=sum(h, mai[c(1,3)]))
    palette(brewer.pal(6, "Set1"))

    d <- data[data$prior == prior,]

    par(las=1, mai=mai, mgp=c(2.25,0.75,0), ps=11, tcl=-0.4)
    plot(d$correlation, d$mse, type="n",
         xlab="Correlation", ylab="CV Error")
    axis(3, label=FALSE)
    axis(4, label=FALSE)

    for (i in seq_len(nlevels(data$centers))) {
        k <- levels(data$centers)[i]
        dk <- data[data$centers == k & data$prior == prior,]
        points(dk$correlation, dk$mse, pch=i, col=i)
    }


    centers <- levels(data$centers)
    ncenters <- nlevels(data$centers)

    usr <- par("usr")
    cxy <- par("cxy")
    legend.x <- usr[2] + 3 * cxy[1]
    legend.y <- usr[3]

    legend(legend.x, legend.y, xjust=0, yjust=0, xpd=TRUE, bty="n",
           title="Clusters", title.adj=0,
           legend=centers, pch=1:ncenters, col=1:ncenters)

    dev.off()
}

