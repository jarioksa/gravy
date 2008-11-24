"GaussPara.HOF" <-
function (resp, model, ...) 
{
    if (missing(model)) 
        model <- pick.model(resp, ...)
    tops <- GaussPara.optop.HOF(resp, model)
    if (!is.na(tops$top))
        tols <- GaussPara.tol.HOF(resp, model, tops$top, tops$opt)
    else
        tols <- list(tol=NA, tol.right = NA)
    out <- list(species = resp$y.name, model = model)
    out$top <- tops$top
    out$opt <- tops$opt
    out$tol <- tols$tol
    out$tol.right <- tols$tol.right
    class(out) <- "GaussPara.HOF"
    out
}
