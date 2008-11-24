"ssHOF" <-
function (x, y, M, model = 5) 
{
    x <- scale01(x)
    if (model >= 4) {
        m2 <- glm(cbind(y, M - y) ~ x + I(x^2), family = binomial)
        k <- coef(m2)
        if (k[3] > 0) {
            m2 <- glm(cbind(y, M-y) ~ x + offset(-x^2), family=binomial)
            k <- c(coef(m2), -1)
        }
        names(k) <- NULL
        u <- -k[2]/2/k[3]
        h <- plogis(k[1] - k[2]^2/4/k[3])
        h <- min(0.98, h)
        u <- min(0.98, u)
        u <- max(0.02, u)
        r <- 1/h * (-2 * h + 2 * sqrt(h))/2
        r <- log(r)
        b <- 5.07 - 0.227 * k[3]
        a <- -b * u + r
        c <- b * u + r
    }
    else if (model > 1) {
        m1 <- glm(cbind(y, M - y) ~ x, family = binomial)
        k <- coef(m1)
        names(k) <- NULL
        a <- -k[1]
        b <- -k[2]
        c <- 0
    }
    switch(model, out <- list(a = log((1 - mean(y/M))/mean(y/M))), 
        out <- list(a = a, b = b), out <- list(a = a, b = b, 
            c = c), out <- list(a = a, b = b, c = c), out <- list(a = a, 
            b = b, c = c, d = b))
    unlist(out)
}
