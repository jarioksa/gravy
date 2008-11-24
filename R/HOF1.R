"HOF1" <-
    function (x, model, p, M = 1, ...) 
{
    model <- match(model, c("I", "II", "III", "IV", "V"))
    x <- scale01(x, ...)
    if (length(M) == 1)
        M <- rep(M, length(x))
    switch(model,
           fv <- M/(1 + exp(p[1])),
           fv <- M/(1 +  exp(p[1] + p[2] * x)),
           fv <- M/(1 + exp(p[1] + p[2] * x))/(1 + exp(p[3])),
           fv <- M/(1 + exp(p[1] + p[2] * x))/(1 + exp(p[3] - p[2] * x)),
           fv <- M/(1 + exp(p[1] + p[2] * x))/(1 + exp(p[3] - p[4] * x)))
    fv
}
