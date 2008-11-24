"print.HOF" <-
    function (x, ...) 
{
    cat("\nCall:\n")
    cat(deparse(x$call), "\n\n")
    cat(x$y.name, "\n")
    p <- coef(x)
    codes <- sapply(x$models, function(x) x$code)
    codes[codes < 3] <- NA
    printCoefmat(cbind(t(p), warnings = codes), na.print="")
    dev <- deviance(x)
    ll <- logLik(x)
    res.df <- df.residual(x)
    df <- sapply(x$models, function(x) length(x$estimate))
    AIC <- 2*ll + 2 * (df)
    BIC <- 2*ll + log(x$nobs) * df
    cat("\n")
    out <- cbind(deviance = dev, AIC = AIC, BIC = BIC, Df = res.df)
    printCoefmat(out, na.print="")
    invisible(x)
}
