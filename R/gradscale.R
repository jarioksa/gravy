"gradscale" <-
    function (resp, grad, ...) 
{
    N <- length(resp)
    M <- length(grad)
    i <- sort.list(grad)
    fv <- rep(0, M)
    df <- rep(0, M)
    resc <- rep(0, M)
    xrange <- resp[[1]]$range
    for (j in 1:N) {
        pick <- pick.model(resp[[j]], ...)
        model <- match(pick, c("I","II","III","IV","V"))
        fv <- predict(resp[[j]], newdata = grad[i], model=pick, ...)
        p <- coef(resp[[j]], model=pick, ...)
        df <- abs(diff(sign(gradder.HOF(model, p, scale01(grad[i])))))
        tmp <- cumsum(abs(diff(fv)))
        if (any(df == 2)) {
            top <- GaussPara(resp[[j]], ...)$top
            itop <- which(df == 2)
            top <- top - max(fv[c(itop, itop + 1)])
            tmp[itop:length(tmp)] <- tmp[itop:length(tmp)] + 
                2 * top
        }
        resc[2:M] <- resc[2:M] + tmp
    }
    out <- resc[sort.list(i)]
    out
}
