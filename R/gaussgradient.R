"gaussgradient" <-
    function (x, grad, family = poisson, xlab, freq.lim = 5, cex.species = 0.7,
              axes = TRUE, ...) 
{
    x <- as.data.frame(x)
    x <- x[, colSums(x>0) >= freq.lim]
    mod <- lapply(x, function(x) glm(x ~ grad + I(grad^2), family=family(link=log)))
    opt <- sapply(mod, function(x) if((p <- coef(x))[3] < 0) -p[2]/2/p[3] else NA)
    tol <- sapply(mod, function(x) if((p <- coef(x))[3] < 0) sqrt(-1/2/p[3]) else NA)
    narrow <- min(tol, na.rm=TRUE)
    ord <- order(opt, na.last = NA)
    x <- x[,ord]
    if (missing(xlab))
        xlab <- deparse(substitute(grad))
    ylab <- names(x)
    plot(range(grad), c(1,length(ord)+1), xlab = xlab, ylab="", axes = FALSE, type="n")
    if (axes) {
        box()  
        axis(1)
        axis(2, at=1:length(ord), ylab, las=1, cex.axis = cex.species)
    }
    xv <- seq(min(grad), max(grad), len=101)
    for (i in 1:length(ord)) {
        fv <- predict(mod[[ord[i]]], new=list(grad=xv), type="response")
        top <- predict(mod[[ord[i]]], new=list(grad=opt[ord[i]]),type="response")
        fv <- 0.95*fv/top*narrow/tol[ord[i]] + i  
        fv <- c(i, fv, i)
        polygon(c(xv[1], xv, xv[length(xv)]), fv, ...)     
    }
    invisible(list(opt=opt, tol=tol))
}

