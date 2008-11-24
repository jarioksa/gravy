"betahill" <-
function (veg, grad, freq.lim=1) 
{
    veg <- as.matrix(veg)
    if (freq.lim > 1) {
        freq <- apply(veg>0, 2, sum)
        veg <- veg[, freq >= freq.lim, drop=FALSE]
    }
    nr <- nrow(veg)
    nc <- ncol(veg)
    tmp <- .C("hill0", as.double(veg), x = as.double(grad), as.integer(nc), 
        as.integer(nr), H1 = double(nr), H2 = double(nr), zv1 = double(20), 
        zv2 = double(20), PACKAGE="gravy")
    zv1 <- c(tmp$zv1[1], tmp$zv1)
    zv2 <- c(tmp$zv2[1], tmp$zv2)
    rug <- seq(min(grad), max(grad), length = 21)
    sol <- list(grad = tmp$x, Hill.1 = tmp$H1, Hill.2 = tmp$H2, 
        zv1 = zv1, zv2 = zv2, rug = rug, cycles = NA)
    sol$gradname <- deparse(substitute(grad))
    sol$call <- match.call()
    class(sol) <- "hillscale"
    sol
}
