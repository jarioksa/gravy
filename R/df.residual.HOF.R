"df.residual.HOF" <-
    function (object, model, ...) 
{
    n <- object$nobs
    out <- n - sapply(object$models, function(x) length(x$estimate))
    if (!missing(model))
        out <- out[model]
    out
}
