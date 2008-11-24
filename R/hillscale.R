"hillscale" <-
function (veg, grad, cycles = 4, freq.lim=1) 
{
    veg <- as.matrix(veg)
    if (freq.lim > 1) {
        fr <- apply(veg>0, 2, sum)
        veg <- veg[, fr >= freq.lim, drop=FALSE]
    }
    nr <- nrow(veg)
    nc <- ncol(veg)
    tmp <- .C("hillstrtch", as.double(veg), x = as.double(grad), 
        as.integer(nc), as.integer(nr), as.integer(cycles), H1 = double(nr), 
        H2 = double(nr), rug = double(21), zv1 = double(45), 
        zv2 = double(45), zn = double(45), PACKAGE="gravy")
    zv1 <- tmp$zv1[1:20]/tmp$zn[1:20]
    zv2 <- tmp$zv2[1:20]/tmp$zn[1:20]
    zv1 <- c(zv1[1], zv1)
    zv2 <- c(zv2[1], zv2)
    sol <- list(grad = tmp$x, Hill.1 = tmp$H1, Hill.2 = tmp$H2, 
        zv1 = zv1, zv2 = zv2, rug = tmp$rug, cycles = cycles)
    sol$gradname <- deparse(substitute(grad))
    sol$call <- match.call()
    class(sol) <- "hillscale"
    sol
}
