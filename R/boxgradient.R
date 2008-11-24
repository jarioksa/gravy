"boxgradient" <-
    function (x, grad, horizontal = TRUE, xlab, freq.lim = 5, cex.species = 0.7,
              axes = TRUE, ...) 
{
    x <- as.data.frame(x)
    x <- x[, colSums(x>0) >= freq.lim]
    mat <- x
    mat[, 1:ncol(mat)] <- grad
    is.na(mat) <- x <= 0
    med <- sapply(mat, median, na.rm = TRUE)
    wav <- apply(x, 2, function(z) weighted.mean(grad,w=z))
    i <- order(med, wav,  na.last = NA)
    mat <- mat[, i]
    all <- boxplot.stats(grad)$stats[2:4]
    if (missing(xlab))
        xlab <- deparse(substitute(grad))
    out <- boxplot(mat, horizontal = horizontal, las = 1, xlab = xlab,
                   axes = FALSE,  ...)
    if (axes) {
        axis(1)
        axis(2, at=1:ncol(mat), label=colnames(mat), las=1, cex.axis = cex.species)
        box()
        rug(grad)
        abline(v = all, lwd=c(1,2,1))
    }
    points(wav[i], 1:length(med), pch=16)
    invisible(out)
}

