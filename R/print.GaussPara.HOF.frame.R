"print.GaussPara.HOF.frame" <-
    function (x, ...) 
{
    nsp <- length(x)
    out <- array(NA, dim = c(nsp, 4))
    rownames(out) <- paste(names(x), sapply(x, function(x) x$model))
    colnames(out) <- c("top", "opt", "tol", "tol.right")
    for (i in 1:nsp) {
        tmp <- x[[i]]
        out[i, 1:3] <- c(tmp$top, tmp$opt, tmp$tol)
        if (tmp$tol.right != "symmetric" && !is.na(tmp$tol.right)) 
            out[i, 4] <- tmp$tol.right
    }
    print(round(out, 3), na.print = "")
    invisible(x)
}
