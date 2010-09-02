`residuals.HOF` <-
    function(object, type = c("deviance", "working", "response"), model, ...)
{
    type <- match.arg(type)
    y <- object$y
    mu <- sapply(object$models, fitted)
    r <- sapply(object$models, residuals)
    res <- switch(type,
                  deviance = ifelse(sweep(mu, 1, y, "<"), sqrt(r), -sqrt(r)),
                  working = r,
                  response = -sweep(mu, 1, y, "-"))
    if (!missing(model))
        res <- res[, model]
    res
}
