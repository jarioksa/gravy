"gradder.HOF" <-
    function (model, p, x, range = 1) 
{
    points <- length(x)
    fit <- rep(NA, points)
    der <- rep(NA, points)
    if (model == 4)
        p[4] <- p[2]
    p[is.na(p)] <- 0
    for (i in 1:points) {
        fit[i] <- .C("HOF", as.integer(model), as.double(p), 
                     as.double(x[i]), f = double(1), PACKAGE="gravy")$f
        der[i] <- .C("dHOF", as.integer(model), as.double(p), 
                     as.double(x[i]), as.double(fit[i]), der = double(1),
                     PACKAGE="gravy")$der
    }
    der/range
}
