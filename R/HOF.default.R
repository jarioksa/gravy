"HOF.default" <-
    function (spec, grad, M, y.name, family=binomial, ...) 
{
    mlHOF <- function (x, y, model, p, M = 1,  ...) 
    {
        mu <- HOF1(x, model, p, M)
        wt <- if(famname == "binomial") M else 1
        if (length(wt) == 1) wt <- rep(wt, length(x))
        n <- wt
        if(famname == "binomial") {
            y <- y/M
            mu <- mu/M
        }
        dev <- sum(dev.resids(y, mu, wt))
        aic(y, n, mu, wt, dev)/2
    }
    aic <- family()$aic
    dev.resids <- family()$dev.resids
    famname <- family()$family
    if (!(famname %in% c("binomial", "gaussian", "poisson")))
        stop("Allowed familes: binomial, gaussian, poisson")
    div <- if(famname == "binomial") M else  1
    wt <- if(famname == "binomial") M else 1
    if (length(wt) == 1)
        wt <- rep(wt, length(grad))
    x.name <- deparse(substitute(grad))
    if (missing(y.name)) 
        y.name <- deparse(substitute(spec))
    x.orig <- grad
    x.range <- range(grad)
    x <- scale01(grad)
    nobs <- length(spec)
    IV.res <- nlm(mlHOF, p = ssHOF(grad, spec, M, 4), x = grad, 
                  y = spec, M = M, model = "IV", ...)
    fv <- HOF1(x, "IV", IV.res$estimate, M, ...)
    IV.res$fitted <- fv
    IV.res$deviance <- sum(dev.resids(spec/div, fv/div, wt))
    III.res <- nlm(mlHOF, p = ssHOF(grad, spec, M, 3), x = grad, 
                   y = spec, M = M, model = "III", ...)
    tmp <- nlm(mlHOF, p = c(IV.res$est[1:2], 0), x = grad, y = spec, 
               M = M, model = "III", ...)
    if (tmp$min < III.res$min) 
        III.res <- tmp
    tmp <- nlm(mlHOF, p = c(IV.res$est[3], -IV.res$est[2], 0), 
               x = grad, y = spec, M = M, model = "III", ...)
    if (tmp$min < III.res$min) 
        III.res <- tmp
    III.res$fitted <- fv <- HOF1(x, "III", III.res$estimate, M, ...)
    III.res$deviance <- sum(dev.resids(spec/div, fv/div, wt))
    V.res <- nlm(mlHOF, p = c(IV.res$est, IV.res$est[2]), x = grad, 
                 y = spec, M = M, model = "V", ...)
    second <- nlm(mlHOF, p = c(III.res$est, 0), x = grad, y = spec, 
                  M = M, model = "V", ...)
    if (second$min < V.res$min) 
        V.res <- second
    V.res$fitted <-
        fv <- HOF1(x, model = "V", V.res$estimate, M, ...)
    V.res$deviance <- sum(dev.resids(spec/div, fv/div, wt))
    II.res <- nlm(mlHOF, p = ssHOF(grad, spec, M, 2), x = grad, 
                  y = spec, M = M, model = "II", ...)
    II.res$fitted <- fv <- HOF1(x, "II", II.res$estimate, M, ...)
    II.res$deviance <- sum(dev.resids(spec/div, fv/div, wt))
    I.res <- nlm(mlHOF, p = ssHOF(grad, spec, M, 1), x = grad, 
                 y = spec, M = M, model = "I", ...)
    I.res$fitted <- fv <- HOF1(x, "I", I.res$estimate, M, ...)
    I.res$deviance <- sum(dev.resids(spec/div, fv/div, wt))
    models <- list(V = V.res, IV = IV.res, III = III.res, II = II.res, 
                   I = I.res)
    out <- list(call = match.call(), x = x.orig, y = spec, x.name = x.name, 
                y.name = y.name, range = x.range, M = M, nobs = nobs, 
                models = models)
    class(out) <- "HOF"
    out
}
