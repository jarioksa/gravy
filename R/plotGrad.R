"plotGrad" <-
    function(x, grad, freq.limit=0, ...)
{
    require(lattice)
    x <- as.matrix(x)
    if (freq.limit) {
        freq <- apply(x > 0, 2, sum)
        x <- x[, freq >= freq.limit, drop=FALSE]
    }
    Response <- as.vector(x)
    Species <- rep(colnames(x), each=nrow(x))
    gradnam <- deparse(substitute(grad))
    Gradient <- rep(grad, ncol(x))
    if (is.factor(grad))
        out <- bwplot(Response ~ Gradient | Species, xlab=gradnam, ...)
    else
        out <- xyplot(Response ~ Gradient | Species, xlab=gradnam, ...)
    out
}
