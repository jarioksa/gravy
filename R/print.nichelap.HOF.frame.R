"print.nichelap.HOF.frame" <-
    function(x, ...)
{
    out <- matrix(0, nrow=length(x), ncol = length(x[[1]]))
    for (i in 1:length(x))
        out[i, ] <- x[[i]]
    rownames(out) <- names(x)
    colnames(out) <- names(x[[1]])
    printCoefmat(out)
    invisible(x)
}
