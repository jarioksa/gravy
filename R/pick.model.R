"pick.model" <-
    function (obj, level = 0.95, test = c("F", "Chisq", "AIC", "BIC"), 
              ...) 
{
    test <- match.arg(test)
    if (test == "F") {
        res.dv <- deviance(obj, "V")
        res.df <- df.residual(obj, "V")
        denom <- res.dv/res.df
        crit <- qf(level, 1, res.dv)
    }
    if (test == "Chisq") {
        crit <- qchisq(level, 1)
        denom <- 1
    }
    if (test == "Chisq" || test == "F") {
        dev <- deviance(obj)
        j <- which.min(dev[2:3]) + 1
        stat <- diff(dev[c(1, j, 4:5)])
        model <- "V"
        for (i in 1:3) {
            if (stat[i]/denom > crit) 
                break
            model <- names(stat)[i]
        }
    }
    else {
        k <- if (test == "BIC") 
            log(obj$nobs)
        else 2
        ic <- 2*logLik(obj) + k * sapply(obj$models, function(x) length(x$estimate))
        model <- (names(ic))[which.min(ic)]
    }
    model
}
