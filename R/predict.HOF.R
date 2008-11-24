"predict.HOF" <-
    function (object, newdata, model, ...) 
{
    p <- coef(object, model, ...)
    xrange <- object$range
    if (missing(newdata)) 
        x <- object$x
    else x <- newdata
    fv <- HOF1(x, model, p, 1, xrange)
    fv
}
