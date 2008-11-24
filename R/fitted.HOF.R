"fitted.HOF" <-
    function (object, model, ...) 
{
    out <- sapply(object$models, function(x) x$fitted)
    if (!missing(model))
        out <- out[,model]
    out
}
