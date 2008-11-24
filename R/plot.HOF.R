"plot.HOF" <-
    function (x, ...) 
{
    best <- pick.model(x, ...)
    drawmods <- c("V", "IV", "III", "II", "I")
    fitwd <- rep(1, 5)
    names(fitwd) <- drawmods
    fitwd[best] <- 4
    i <- order(x$x)
    fv <- fitted(x)
    fv <- sweep(fv, 1, x$M, "/")
    plot(x$x, x$y/x$M, xlab = x$x.name, ylab = x$y.name, pch = "+", ...)
    matlines(x$x[i], fv[i,], lty=1, lwd=fitwd, ...)
    invisible()
}
