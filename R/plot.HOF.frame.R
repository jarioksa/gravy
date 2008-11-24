`plot.HOF.frame` <-
    function (x, level = 0.95, test = "F", species, ...) 
{
    mods <- x
    require(lattice)
    N <- length(mods)
    nobs <- length(mods[[1]]$x)
    if (missing(species)) 
        species <- names(mods)
    Species <- rep(species, each=nobs)
    Species <- factor(Species, levels=species)
    mods <- mods[names(mods) %in% species]
    Response <- unlist(lapply(mods, function(x) x$y/x$M))
    Gradient <- unlist(lapply(mods, function(x) x$x))
    Fit <- unlist(lapply(mods, function(x) fitted(x, model = pick.model(x, 
                                                     test = test))/x$M))
    mod <- match(sapply(mods, pick.model, test = test), c("I", 
                                          "II", "III", "IV", "V"))
    mod <- rep(mod, each = nobs)
    fit.panel <- function(x, y, subscripts, Fit, ...) {
        panel.xyplot(x, y, ...)
        i <- order(x)
        fv <- Fit[subscripts]
        sp <- unique(cbind(x[i], fv[i]))
        panel.xyplot(sp[, 1], sp[, 2], type = "l", col = cols[mod[min(subscripts)]], 
                     ...)
    }
    cols <- trellis.par.get("superpose.line")$col
    mykey <- list(text = list(text = c("I", "II", "III", "IV", 
                              "V")), lines = list(lty = 1, col = cols[1:5]), columns = 5)
    out <- xyplot(Response ~ Gradient | Species, xlab = mods[[1]]$x.name, 
                  Fit = Fit, key = mykey, panel = fit.panel, ...)
    out
}
