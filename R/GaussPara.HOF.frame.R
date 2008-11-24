"GaussPara.HOF.frame" <-
    function (resp, ...) 
{
    out <- lapply(resp, GaussPara, ...)
    class(out) <- "GaussPara.HOF.frame"
    out
}
