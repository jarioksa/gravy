"betadiversity.HOF.frame" <-
    function (object, x, ...) 
{
    points <- length(x)
    nsp <- length(object)
    beta <- rep(0, points)
    xrange <- object[[1]]$range
    grad <- scale01(x, xrange)
    for (j in 1:nsp) {
        pick <- pick.model(object[[j]], ...)
        model <- match(pick, c("I","II","III","IV","V"))
        p <- coef(object[[j]], pick)
        beta <- beta + abs(gradder.HOF(model, p, grad, diff(xrange)))
    }
    sol <- list(x = x, beta = beta)
    class(sol) <- "betadiversity"
    sol
}
