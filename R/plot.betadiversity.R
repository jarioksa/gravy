"plot.betadiversity" <-
function (x, type = "b", xlab, ylab, splines = FALSE,  ...) 
{
    i <- order(x$x)
    if (missing(xlab)) 
        xlab <- "Gradient"
    if (missing(ylab)) 
        ylab <- "Beta Diversity"
    i <- order(x$x)
    xy <- unique(cbind(x$x[i], x$beta[i]))
    plot(xy, type = "n", xlab = xlab, ylab = ylab, ylim = c(0, 
        max(x$beta)), ...)
    if (type == "b" || type == "p") 
        points(xy, ...)
    if (type == "b" || type == "l") {
        if (splines)
            lines(spline(xy), ...)
        else 
            lines(xy, ...)
    }
    invisible()
}
