"deviance.HOF" <-
    function (object, model, ...) 
{
    out <- sapply(object$models, function(x) x$deviance)
    if (!missing(model))
        out <- out[model]
    out
}
