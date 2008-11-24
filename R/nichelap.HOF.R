"nichelap.HOF" <-
function (sp1, sp2, test = "BIC", ...) 
{
    m1 <- pick.model(sp1, test = test)
    m2 <- pick.model(sp2, test = test)
    p1 <- coef(sp1, m1)
    p2 <- coef(sp2, m2)
    HOFp <- function(x, model, p, ...) {
        model <- match(model, c("I", "II", "III", "IV", "V"))
        switch(model, fv <- rep(1/(1 + exp(p[1])), length(x)), fv <- 1/(1 + exp(p[1] + 
            p[2] * x)), fv <- 1/(1 + exp(p[1] + p[2] * x))/(1 + 
            exp(p[3])), fv <- 1/(1 + exp(p[1] + p[2] * x))/(1 + 
            exp(p[3] - p[2] * x)), fv <- 1/(1 + exp(p[1] + p[2] * 
            x))/(1 + exp(p[3] - p[4] * x)))
        fv
    }
    area1 <- integrate(HOFp, 0, 1, p = p1, model = m1)$value
    area2 <- integrate(HOFp, 0, 1, p = p2, model = m2)$value
    lap <- function(x, p1, p2, m1, m2) pmin(HOFp(x, m1, p1), 
        HOFp(x, m2, p2))
    niche <- integrate(lap, 0, 1, p1 = p1, p2 = p2, m1 = m1, 
        m2 = m2)$value
    out <- c(area1, area2, niche, niche/(area1 + area2 - niche), 
        niche/area1, niche/area2)
    names(out) <- c("species 1", "species 2", "overlap", "prop.total", 
        "prop. sp1", "prop.sp2")
    out
}
