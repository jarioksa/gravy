"HOF.data.frame" <-
function (veg, grad, M, freq.limit = 10, ...) 
{
    freq <- apply(veg > 0, 2, sum)
    veg <- veg[, freq >= freq.limit, drop = FALSE]
    out <- lapply(veg, HOF, grad = grad, M = M, ...)
    class(out) <- "HOF.frame"
    out
}
