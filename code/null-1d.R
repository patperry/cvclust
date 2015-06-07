# code/null-1d.R
# experiments for the null case (1 cluster) in one dimesion

library("grid")
library("MASS") # lda
library("RColorBrewer") # brewer.pal

palette(brewer.pal(6, "Set1"))


# default seed chosen by random.org
cluster <- function(x, centers, seed = 2651513, nstart = 100, ...)
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





set.seed(0)

ntrain <- 1000
ntest <- ntrain
n <- ntest + ntrain
train <- seq_len(ntrain)
test <- ntrain + seq_len(ntest)

predictor <- 1
response <- 2

z <- matrix(rnorm(n * 2), n, 2)

cverr <- function(correlation, centers)
{
    r <- correlation
    coef <- cbind(rbind(sqrt((1 + r) / 2),  sqrt((1 - r) / 2)),
                  rbind(sqrt((1 + r) / 2), -sqrt((1 - r) / 2)))
    x <- z %*% coef

    if (centers == 1) {
        fit <- colMeans(x[train,response,drop=FALSE])
        err <- scale(x[test,response], center=fit, scale=FALSE)
    } else {
        cl <- cluster(x[train,response], centers=centers)
        cluster <- factor(cl$cluster, levels=seq_len(centers))
        fit <- lda(x[train,predictor,drop=FALSE], cluster)
        pred <- predict(fit, x[test,predictor,drop=FALSE])$class
        err <- x[test,response] - cl$centers[pred,]
    }

    mse <- mean(err^2)
    mse
}


correlation <- seq(-1, 1, length.out=21)
mse1 <- sapply(correlation, function(r) cverr(r, 1))
mse2 <- sapply(correlation, function(r) cverr(r, 2))
mse3 <- sapply(correlation, function(r) cverr(r, 3))
mse4 <- sapply(correlation, function(r) cverr(r, 4))
mse8 <- sapply(correlation, function(r) cverr(r, 8))
mse15 <- sapply(correlation, function(r) cverr(r, 15))

data <- data.frame(correlation = rep(correlation, 6),
                   mse = c(mse1, mse2, mse3, mse4, mse8, mse15),
                   centers = factor(rep(c(1, 2, 3, 4, 8, 15),
                                    each=length(correlation))))

h.in <- 4
w.in <- 4
vplay <- grid.layout(1, 2,
                     widths=unit(w.in * c(1, 0.5), "in"),
                     heights=unit(h.in, "in"))




gp <- gpar(fontsize=11)

gplot <- local({
    g <- gTree(x = NULL, y = NULL,
        gp = gp,
        childrenvp = vpTree(
            plotViewport(c(3.25, 3.25, 0.75, 0.75), name="plotRegion"),
            vpList(dataViewport(data$correlation, data$mse,
                                width=unit(3, "in"),
                                height=unit(3, "in"),
                                name="dataRegion"))),
        children = gList(
            xaxisGrob(vp="plotRegion::dataRegion"),
            xaxisGrob(vp="plotRegion::dataRegion", main=FALSE, label=FALSE),
            yaxisGrob(vp="plotRegion::dataRegion"),
            yaxisGrob(vp="plotRegion::dataRegion", main=FALSE, label=FALSE),
            textGrob(vp="plotRegion::dataRegion",
                     "Correlation", y=unit(-2.75, "lines")),
            textGrob(vp="plotRegion::dataRegion",
                     "CV Error", x=unit(-2.75, "lines"), rot=90),
            rectGrob(vp="plotRegion::dataRegion")))


    for (i in seq_len(nlevels(data$centers))) {
        k <- levels(data$centers)[i]
        dk <- subset(data, centers == k)
        g <- addGrob(g, pointsGrob(vp="plotRegion::dataRegion",
                                    dk$correlation, dk$mse,
                                    size=unit(6, "points"),
                                    pch=i, gp=gpar(col=i)))
    }

    g
})



glegend <- local({
    centers <- levels(data$centers)
    ncenters <- nlevels(data$centers)

    title <- textGrob("Clusters")
    entries <- legendGrob(centers, ncol=1, pch=seq_len(ncenters),
                          hgap = unit(0.50, "lines"),
                          vgap = unit(0.25, "lines"),
                          gp=gpar(col=seq_len(ncenters)))
    entries <- editGrob(entries, gPath("points"), size=unit(6, "points"),
                        grep=TRUE, global=TRUE)
    gf <- frameGrob()
    gf <- packGrob(gf, title, side="bottom",
                   border=unit(c(0.25, 0.25, 0.25, 0.25), "lines"))
    gf <- packGrob(gf, entries, side="bottom",
                   border=unit(c(0.25, 0.25, 0.25, 0.25), "lines"))
    #gf <- packGrob(gf, rectGrob())
    gTree(gp=gp, children=gList(gf))
})



gf <- frameGrob()
gf <- packGrob(gf, gplot, row=1, col=1)
gf <- packGrob(gf, glegend, row=1, col=2,
               width=unit(strwidth("Clusters", "in"), "in")
                     + unit(1, "lines"))


pdf("fig.pdf",
    width=sum(as.numeric(convertUnit(vplay$widths, "in"))),
    height=sum(as.numeric(convertUnit(vplay$heights, "in"))))
grid.newpage()
grid.draw(gf)

#abline(h=1, col=3)
#lines(correlation, 1 + (2/pi) * (1 - 2 * abs(correlation)), col=2)
