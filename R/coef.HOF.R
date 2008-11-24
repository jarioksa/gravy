"coef.HOF" <-
    function (object, model, ...) 
{
    out <- sapply(object$models, function(x)
                  c(x$estimate, rep(NA, 4 - length(x$estimate))))
    rownames(out) <- letters[1:4]
    if (!missing(model)) {
        out <- out[,model]
    }
    out
}
