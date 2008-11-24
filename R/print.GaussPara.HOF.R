"print.GaussPara.HOF" <-
function (x, ...) 
{
    out <- array(NA, dim=c(1,5))
    rownames(out) <- x$species
    colnames(out) <- names((unlist(x)))[2:6]
    out[1, 1] <- x$model
    if (!is.na(x$top)) 
        out[1, 2:4] <- c(round(x$top,3), round(x$opt,3), round(x$tol,3))
    else out[1, 2:4] <- NA
    if (!is.na(x$tol.right) && is.numeric(x$tol.right)) 
        out[1, 5] <- round(x$tol.right, 3)
    else 
        out[1, 5] <- NA
    print(out, quote = FALSE, na.print = "")
    invisible(out)
}
