"nichelap.HOF.frame" <-
    function(df, test="BIC", ...)
{
    nsp <- length(df)
    spnam <-  names(df)
    out <- list()
    for (i in 1:(nsp-1)) {
        n1 <- spnam[i]
        for(j in (i+1):nsp) {
            n2 <- spnam[j]
            out[[paste(n1, n2)]] <-
                nichelap.HOF(df[[i]], df[[j]], test=test)
        }
    }
    attr(out, "species") <- spnam
    class(out) <- "nichelap.HOF.frame"
    out
}
