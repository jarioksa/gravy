"GaussPara.optop.HOF" <-
    function (resp, model, ...) 
{
    if (model == "IV") {
        p <- coef(resp, model)
        minx <- resp$range[1]
        ranx <- diff(resp$range)
        opt <- (p[3] - p[1])/p[2]/2
        opt <- ranx * opt + minx
        top <- predict(resp, new = opt, M = 1, model = "IV")
    }
    else if (model == "V") {
        p <- coef(resp, model)
        if (p[2] * p[4] >= 0) {
            top <- 0
            HOFfun <- function(x, resp) predict(resp, new = x, 
                                                M = 1, model = "V")
            tmp <- optimize(HOFfun, resp$range, resp = resp, 
                            maximum = TRUE)
            opt <- tmp$max
            top <- tmp$obj
            if (top < 16 * .Machine$double.eps) {
                tmp <- seq(resp$range[1], resp$range[2], len = 31)
                ytmp <- predict(resp, new = tmp, model = "V")
                tmp <- tmp[ytmp > 16 * .Machine$double.eps]
                tmp <- optimize(HOFfun, range(tmp), resp = resp, 
                                maximum = TRUE)
                opt <- tmp$max
                top <- tmp$obj
                if (tmp$obj <= 0) 
                    opt <- top <- NA
            }
        }
        else opt <- top <- NA
    }
    else {
        opt <- top <- NA
    }
    list(opt = opt, top = top)
}
