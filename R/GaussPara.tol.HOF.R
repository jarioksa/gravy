"GaussPara.tol.HOF" <-
function (resp, model, top, opt, ...) 
{
    if (model == "III" || model == "II" || model == "I") {
        return(list(tol = NA, tol.right = NA))
    }
    HOFfun <- function(x, resp, model, val) predict(resp, new = x, 
        M = 1, model = model) - val
    if (missing(top) || missing(opt)) {
        tmp <- GaussPara.optop.HOF(resp, model)
        opt <- tmp$opt
        top <- tmp$top
    }
    y.tol <- exp(-0.5) * top
    ranx <- diff(resp$range)
    step <- ranx/4
    fork <- opt - step
    limx <- 64 * ranx
    while (HOFfun(fork, resp, model, y.tol) > 0 && step <= limx) {
        fork <- fork - step
        step <- 2 * step
    }
    tol <- opt - uniroot(HOFfun, c(fork, opt), resp = resp, model = model, 
        val = y.tol)$root
    tol.right <- "symmetric"
    if (model == "V") {
        step <- ranx/4
        fork <- opt + step
        while (HOFfun(fork, resp, model, y.tol) > 0 && fork <= limx ) {
            fork <- fork + step
            step <- 2 * step
        }
        tol.right <- uniroot(HOFfun, c(opt, fork), resp = resp, 
            model = model, val = y.tol)$root
        tol.right <- tol.right - opt
    }
    list(tol = tol, tol.right = tol.right)
}
