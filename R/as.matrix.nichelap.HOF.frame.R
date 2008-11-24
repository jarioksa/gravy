"as.matrix.nichelap.HOF.frame" <-
    function(x, ...)
{
    spnam <- attr(x, "species")
    N <- length(spnam)
    out <- matrix(0, nrow=N, ncol=N)
    rownames(out) <- colnames(out) <- spnam
    now <- 1
    for (i in 1:(N-1)) {
        for (j in (i+1):N) {
            out[i, j] <- (x[[now]])[5]
            out[j, i] <- (x[[now]])[6]
            now <- now+1
        }
    }
    diag(out) <- NA
    out
}
