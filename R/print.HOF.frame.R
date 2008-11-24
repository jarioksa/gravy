"print.HOF.frame" <-
    function (x, ...) 
{
    cat("Deviances:\n")
    printCoefmat(sapply(x, deviance), na.print="", ...)
    cat("\nSuggested `best' models:\n")
    tmp <- sapply(x, pick.model, ...)
    names(tmp) <- names(x)
    print(noquote(tmp))
    invisible(x)
}
