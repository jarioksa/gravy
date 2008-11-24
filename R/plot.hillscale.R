"plot.hillscale" <-
    function (x, which=c(1,2), xlab, ...) 
{
    op <- par(no.readonly=TRUE)
    if (prod(op$mfrow) < length(which))
        par(ask=TRUE)
    zx <- seq(min(x$grad), max(x$grad), len=21)
    if (missing(xlab)) {
        xlab <- x$gradname
        if (!is.na(x$cycles))
            xlab <- paste(xlab, " - ", x$cycles, "Hill scalings")
    }
    if (1 %in% which) {
        plot(x$grad, x$Hill.1, xlab=xlab, ylab="Hill 1", ...)
        lines(zx, x$zv1, type="S", lwd=2, ...)
        rug(x$rug)
        abline(h=1, lty=3)
    }
    if (2 %in% which) {
        plot(x$grad, x$Hill.2, xlab=xlab, ylab="Hill 2",...)
        lines(zx, x$zv2, type="S", lwd=2, ...)
        rug(x$rug)
        abline(h=1, lty=3)
    }
    par(ask=op$ask)
    invisible() 
}
