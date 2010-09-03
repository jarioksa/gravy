`residuals.HOF` <-
    function(object,
             type = c("deviance", "working", "response", "pearson"),
             model, ...)
{
    type <- match.arg(type)
    y <- object$y
    V <- object$family$variance
    M <- object$M
    if (length(M) == 1)
        M <- rep(M, length(y))
    mu <- sapply(object$models, fitted)
    r <- sapply(object$models, residuals)
    res <- switch(type,
                  deviance = ifelse(sweep(mu, 1, y, "<"), sqrt(r), -sqrt(r)),
                  working = r,
                  response = -sweep(mu, 1, y, "-"),
                  pearson = -sweep(mu, 1, y, "-")/V(sweep(mu, 1, M, "/")))
    if (!missing(model))
        res <- res[, model]
    res
}
